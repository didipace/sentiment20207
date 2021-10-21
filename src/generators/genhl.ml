(*
 * Copyright (C)2005-2015 Haxe Foundation
 *
 * Permission is hereby granted, free of charge, to any person obtaining a
 * copy of this software and associated documentation files (the "Software"),
 * to deal in the Software without restriction, including without limitation
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,
 * and/or sell copies of the Software, and to permit persons to whom the
 * Software is furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
 * DEALINGS IN THE SOFTWARE.
 *)
open Extlib_leftovers
open Unix
open Globals
open Ast
open Type
open Common
open Hlcode

(* compiler *)

type ('a,'b) lookup = {
	arr : 'b DynArray.t;
	mutable map : ('a, int) PMap.t;
}

(* not mutable, might be be shared *)
type method_capture = {
	c_map : (int, int) PMap.t;
	c_vars : tvar array;
	c_type : ttype;
	c_group : bool;
}

type allocator = {
	mutable a_all : int list;
	mutable a_hold : int list;
}

type lassign = (string index * int)

type method_context = {
	mid : int;
	mregs : (int, ttype) lookup;
	mops : opcode DynArray.t;
	mret : ttype;
	mdebug : Globals.pos DynArray.t;
	mvars : (int, int) Hashtbl.t;
	mhasthis : bool;
	mutable mdeclared : int list;
	mutable mallocs : (ttype, allocator) PMap.t;
	mutable mcaptured : method_capture;
	mutable mcontinues : (int -> unit) list;
	mutable mbreaks : (int -> unit) list;
	mutable mtrys : int;
	mutable mloop_trys : int;
	mutable mcaptreg : int;
	mutable mcurpos : Globals.pos;
	mutable massign : lassign list;
}

type array_impl = {
	aall : tclass;
	abase : tclass;
	adyn : tclass;
	aobj : tclass;
	aui16 : tclass;
	ai32 : tclass;
	af32 : tclass;
	af64 : tclass;
}

type constval =
	| CString of string

type context = {
	com : Common.context;
	cglobals : (string, ttype) lookup;
	cstrings : (string, string) lookup;
	cbytes : (bytes, bytes) lookup;
	cfloats : (float, float) lookup;
	cints : (int32, int32) lookup;
	cnatives : (string * int, (string index * string index * ttype * functable index)) lookup;
	cfids : (string * path, unit) lookup;
	cfunctions : fundecl DynArray.t;
	cconstants : (constval, (global * int array)) lookup;
	optimize : bool;
	overrides : (string * path, bool) Hashtbl.t;
	defined_funs : (int,unit) Hashtbl.t;
	is_macro : bool;
	mutable dump_out : (unit IO.output) option;
	mutable cached_types : (string list, ttype) PMap.t;
	mutable m : method_context;
	mutable anons_cache : (tanon, ttype) PMap.t;
	mutable method_wrappers : ((ttype * ttype), int) PMap.t;
	mutable rec_cache : (Type.t * ttype option ref) list;
	mutable cached_tuples : (ttype list, ttype) PMap.t;
	mutable tstring : ttype;
	macro_typedefs : (string, ttype) Hashtbl.t;
	array_impl : array_impl;
	base_class : tclass;
	base_type : tclass;
	base_enum : tclass;
	core_type : tclass;
	core_enum : tclass;
	ref_abstract : tabstract;
	cdebug_files : (string, string) lookup;
	mutable ct_delayed : (unit -> unit) list;
	mutable ct_depth : int;
}

(* --- *)

type access =
	| ANone
	| AGlobal of global
	| ALocal of tvar * reg
	| AStaticVar of global * ttype * field index
	| AStaticFun of fundecl index
	| AInstanceFun of texpr * fundecl index
	| AInstanceProto of texpr * field index
	| AInstanceField of texpr * field index
	| AArray of reg * (ttype * ttype) * reg
	| AVirtualMethod of texpr * field index
	| ADynamic of texpr * string index
	| AEnum of tenum * field index
	| ACaptured of field index

let is_to_string t =
	match follow t with
	| TFun([],r) -> (match follow r with TInst({ cl_path=[],"String" },[]) -> true | _ -> false)
	| _ -> false

let is_string = function
	| HObj { pname = "String"} -> true
	| _ -> false

let is_extern_field f =
	not (Type.is_physical_field f) || (match f.cf_kind with Method MethNormal -> List.exists (fun (m,_,_) -> m = Meta.HlNative) f.cf_meta | _ -> false) || has_class_field_flag f CfExtern

let is_array_class name =
	match name with
	| "hl.types.ArrayDyn" | "hl.types.ArrayBytes_Int" | "hl.types.ArrayBytes_Float" | "hl.types.ArrayObj" | "hl.types.ArrayBytes_hl_F32" | "hl.types.ArrayBytes_hl_UI16" -> true
	| _ -> false

let is_array_type t =
	match t with
	| HObj p -> is_array_class p.pname
	| _ -> false

let max_pos e =
	let p = e.epos in
	{ p with pmin = p.pmax }

let to_utf8 str p =
	let u8 = try
		UTF8.validate str;
		str;
	with
		UTF8.Malformed_code ->
			(* ISO to utf8 *)
			let b = UTF8.Buf.create 0 in
			String.iter (fun c -> UTF8.Buf.add_char b (UCharExt.of_char c)) str;
			UTF8.Buf.contents b
	in
	let ccount = ref 0 in
	UTF8.iter (fun c ->
		let c = UCharExt.code c in
		if (c >= 0xD800 && c <= 0xDFFF) || c >= 0x110000 then abort "Invalid unicode char" p;
		incr ccount;
		if c >= 0x10000 then incr ccount;
	) u8;
	u8, !ccount

let tuple_type ctx tl =
	try
		PMap.find tl ctx.cached_tuples
	with Not_found ->
		let ct = HEnum {
			eglobal = None;
			ename = "";
			eid = 0;
			efields = [|"",0,Array.of_list tl|];
		} in
		ctx.cached_tuples <- PMap.add tl ct ctx.cached_tuples;
		ct

let type_size_bits = function
	| HUI8 | HBool -> 0
	| HUI16 -> 1
	| HI32 | HF32 -> 2
	| HI64 | HF64 -> 3
	| _ -> die "" __LOC__

let new_lookup() =
	{
		arr = DynArray.create();
		map = PMap.empty;
	}

let null_capture =
	{
		c_vars = [||];
		c_map = PMap.empty;
		c_type = HVoid;
		c_group = false;
	}

let lookup l v fb =
	try
		PMap.find v l.map
	with Not_found ->
		let id = DynArray.length l.arr in
		DynArray.add l.arr (Obj.magic 0);
		l.map <- PMap.add v id l.map;
		DynArray.set l.arr id (fb());
		id

let lookup_alloc l v =
	let id = DynArray.length l.arr in
	DynArray.add l.arr v;
	id

let method_context id t captured hasthis =
	{
		mid = id;
		mregs = new_lookup();
		mops = DynArray.create();
		mvars = Hashtbl.create 0;
		mallocs = PMap.empty;
		mret = t;
		mbreaks = [];
		mdeclared = [];
		mcontinues = [];
		mhasthis = hasthis;
		mcaptured = captured;
		mtrys = 0;
		mloop_trys = 0;
		mcaptreg = 0;
		mdebug = DynArray.create();
		mcurpos = Globals.null_pos;
		massign = [];
	}

let field_name c f =
	s_type_path c.cl_path ^ ":" ^ f.cf_name

let efield_name e f =
	s_type_path e.e_path ^ ":" ^ f.ef_name

let global_type ctx g =
	DynArray.get ctx.cglobals.arr g

let is_overridden ctx c f =
	ctx.is_macro || Hashtbl.mem ctx.overrides (f.cf_name,c.cl_path)

let alloc_float ctx f =
	lookup ctx.cfloats f (fun() -> f)

let alloc_i32 ctx i =
	lookup ctx.cints i (fun() -> i)

let alloc_string ctx s =
	lookup ctx.cstrings s (fun() -> s)

let alloc_bytes ctx s =
	lookup ctx.cbytes s (fun() -> s)

let array_class ctx t =
	match t with
	| HI32 ->
		ctx.array_impl.ai32
	| HUI16 ->
		ctx.array_impl.aui16
	| HF32 ->
		ctx.array_impl.af32
	| HF64 ->
		ctx.array_impl.af64
	| HDyn ->
		ctx.array_impl.adyn
	| _ ->
		ctx.array_impl.aobj

let member_fun c t =
	match follow t with
	| TFun (args, ret) -> TFun (("this",false,TInst(c,[])) :: args, ret)
	| _ -> die "" __LOC__

let rec unsigned t =
	match follow t with
	| TAbstract ({ a_path = [],"UInt" },_) -> true
	| TAbstract (a,pl) -> unsigned (Abstract.get_underlying_type a pl)
	| _ -> false

let unsigned_op e1 e2 =
	let is_unsigned e =
		match e.eexpr with
		| TConst (TInt _) -> true
		| _ -> unsigned e.etype
	in
	is_unsigned e1 && is_unsigned e2

let set_curpos ctx p =
	ctx.m.mcurpos <- p

let make_debug ctx arr =
	let get_relative_path p =
		match Common.defined ctx.com Common.Define.AbsolutePath with
		| true -> if (Filename.is_relative p.pfile)
			then Filename.concat (Sys.getcwd()) p.pfile
			else p.pfile
		| false -> try
			(* lookup relative path *)
			let len = String.length p.pfile in
			let base = List.find (fun path ->
				let l = String.length path in
				len > l && String.sub p.pfile 0 l = path
			) ctx.com.Common.class_path in
			let l = String.length base in
			String.sub p.pfile l (len - l)
		with Not_found ->
			p.pfile
	in
	let pos = ref (0,0) in
	let cur_file = ref 0 in
	let cur_line = ref 0 in
	let cur = ref Globals.null_pos in
	let out = Array.make (DynArray.length arr) !pos in
	for i = 0 to DynArray.length arr - 1 do
		let p = DynArray.unsafe_get arr i in
		if p != !cur then begin
			let file = if p.pfile == (!cur).pfile then !cur_file else lookup ctx.cdebug_files p.pfile (fun() -> if ctx.is_macro then p.pfile else get_relative_path p) in
			let line = if ctx.is_macro then p.pmin lor ((p.pmax - p.pmin) lsl 20) else Lexer.get_error_line p in
			if line <> !cur_line || file <> !cur_file then begin
				cur_file := file;
				cur_line := line;
				pos := (file,line);
			end;
			cur := p;
		end;
		Array.unsafe_set out i !pos
	done;
	out

let fake_tnull =
	{null_abstract with
		a_path = [],"Null";
		a_params = [{ttp_name = "T"; ttp_type = t_dynamic; ttp_default = None}];
	}

let get_rec_cache ctx t none_callback not_found_callback =
	try
		match !(List.assq t ctx.rec_cache) with
		| None -> none_callback()
		| Some t -> t
	with Not_found ->
		let tref = ref None in
		ctx.rec_cache <- (t,tref) :: ctx.rec_cache;
		let t = not_found_callback tref in
		ctx.rec_cache <- List.tl ctx.rec_cache;
		t

let rec to_type ?tref ctx t =
	match t with
	| TMono r ->
		(match r.tm_type with
		| None -> HDyn
		| Some t -> to_type ?tref ctx t)
	| TType (td,tl) ->
		let t =
			get_rec_cache ctx t
				(fun() -> abort "Unsupported recursive type" td.t_pos)
				(fun tref -> to_type ~tref ctx (apply_typedef td tl))
		in
		(match td.t_path with
		| ["haxe";"macro"], name -> Hashtbl.replace ctx.macro_typedefs name t; t
		| _ -> t)
	| TLazy f ->
		to_type ?tref ctx (lazy_type f)
	| TFun (args, ret) ->
		HFun (List.map (fun (_,o,t) ->
			let pt = to_type ctx t in
			if o && not (is_nullable pt) then HRef pt else pt
		) args, to_type ctx ret)
	| TAnon a when (match !(a.a_status) with Statics _ | EnumStatics _ -> true | _ -> false) ->
		(match !(a.a_status) with
		| Statics c ->
			class_type ctx c (extract_param_types c.cl_params) true
		| EnumStatics e ->
			enum_class ctx e
		| _ -> die "" __LOC__)
	| TAnon a ->
		if PMap.is_empty a.a_fields then HDyn else
		(try
			(* can't use physical comparison in PMap since addresses might change in GC compact,
				maybe add an uid to tanon if too slow ? *)
			PMap.find a ctx.anons_cache
		with Not_found ->
			let vp = {
				vfields = [||];
				vindex = PMap.empty;
			} in
			let t = HVirtual vp in
			(match tref with
			| None -> ()
			| Some r -> r := Some t);
			ctx.anons_cache <- PMap.add a t ctx.anons_cache;
			let fields = PMap.fold (fun cf acc -> cfield_type ctx cf :: acc) a.a_fields [] in
			let fields = List.sort (fun (n1,_,_) (n2,_,_) -> compare n1 n2) fields in
			vp.vfields <- Array.of_list fields;
			Array.iteri (fun i (n,_,_) -> vp.vindex <- PMap.add n i vp.vindex) vp.vfields;
			t
		)
	| TDynamic _ ->
		HDyn
	| TEnum (e,_) ->
		enum_type ~tref ctx e
	| TInst ({ cl_path = ["hl"],"Abstract" },[TInst({ cl_kind = KExpr (EConst (String(name,_)),_) },_)]) ->
		HAbstract (name, alloc_string ctx name)
	| TInst (c,pl) ->
		(match c.cl_kind with
		| KTypeParameter tl ->
			let rec loop = function
				| [] -> HDyn
				| t :: tl ->
					match follow (apply_params c.cl_params pl t) with
					| TInst (c,_) as t when not (has_class_flag c CInterface) -> to_type ?tref ctx t
					| _ -> loop tl
			in
			loop tl
		| _ -> class_type ~tref ctx c pl false)
	| TAbstract ({a_path = [],"Null"},[t1]) ->
		let t = to_type ?tref ctx t1 in
		if not (is_nullable t) && t <> HVoid then HNull t else t
	| TAbstract (a,pl) ->
		if Meta.has Meta.CoreType a.a_meta then
			(match a.a_path with
			| [], "Void" -> HVoid
			| [], "Int" | [], "UInt" -> HI32
			| [], "Float" -> HF64
			| [], "Single" -> HF32
			| [], "Bool" -> HBool
			| [], "Dynamic" -> HDyn
			| [], "Class" ->
				class_type ctx ctx.base_class [] false
			| [], "Enum" ->
				class_type ctx ctx.base_type [] false
			| [], "EnumValue" -> HDyn
			| ["hl"], "Ref" -> HRef (to_type ctx (List.hd pl))
			| ["hl"], ("Bytes" | "BytesAccess") -> HBytes
			| ["hl"], "Type" -> HType
			| ["hl"], "UI16" -> HUI16
			| ["hl"], "UI8" -> HUI8
			| ["hl"], "I64" -> HI64
			| ["hl"], "NativeArray" -> HArray
			| ["haxe";"macro"], "Position" -> HAbstract ("macro_pos", alloc_string ctx "macro_pos")
			| _ -> failwith ("Unknown core type " ^ s_type_path a.a_path))
		else
			get_rec_cache ctx t
				(fun() -> HDyn)
				(fun tref -> to_type ~tref ctx (Abstract.get_underlying_type a pl))

and resolve_class ctx c pl statics =
	let not_supported() =
		failwith ("Extern type not supported : " ^ s_type (print_context()) (TInst (c,pl)))
	in
	match c.cl_path, pl with
	| ([],"Array"), [t] ->
		if statics then ctx.array_impl.abase else array_class ctx (to_type ctx t)
	| ([],"Array"), [] ->
		die "" __LOC__
	| _, _ when (has_class_flag c CExtern) ->
		not_supported()
	| _ ->
		c

and cfield_type ctx cf =
	let t = to_type ctx cf.cf_type in
	let t = (match cf.cf_kind, t with
		| Method (MethNormal|MethInline), HFun (args,ret) -> HMethod (args,ret)
		| _ -> t
	) in
	(cf.cf_name,alloc_string ctx cf.cf_name,t)

and field_type ctx f p =
	match f with
	| FInstance (c,pl,f) | FClosure (Some (c,pl),f) ->
		let creal = resolve_class ctx c pl false in
		let rec loop c =
			try
				PMap.find f.cf_name c.cl_fields
			with Not_found ->
				match c.cl_super with
				| Some (csup,_) -> loop csup
				| None -> abort (s_type_path creal.cl_path ^ " is missing field " ^ f.cf_name) p
		in
		(loop creal).cf_type
	| FStatic (_,f) | FAnon f | FClosure (_,f) -> f.cf_type
	| FDynamic _ -> t_dynamic
	| FEnum (_,f) -> f.ef_type

and real_type ctx e =
	let rec loop e =
		match e.eexpr with
		| TField (_,f) ->
			let ft = field_type ctx f e.epos in
			(match ft, e.etype with
			| TFun (args,ret), TFun (args2,_) ->
				TFun (List.map2 (fun ((name,opt,t) as a) ((_,_,t2) as a2) ->
					match t, t2 with
					(*
						Handle function variance:
						If we have type parameters which are function types, we need to keep the functions
						because we might need to insert a cast to coerce Void->Bool to Void->Dynamic for instance.
					*)
					| TInst ({cl_kind=KTypeParameter _},_), TFun _ -> a2
					(*
						If we have a number, it is more accurate to cast it to the type parameter before wrapping it as dynamic
						Ignore dynamic method (#7166)
					*)
					| TInst ({cl_kind=KTypeParameter _},_), t when is_number (to_type ctx t) && (match f with FInstance (_,_,{ cf_kind = Var _ | Method MethDynamic }) -> false | _ -> true) ->
						(name, opt, TAbstract (fake_tnull,[t]))
					| _ ->
						a
				) args args2, ret)
			| _ -> ft)
		| TLocal v -> v.v_type
		| TParenthesis e -> loop e
		| TArray (arr,_) ->
			let rec loop t =
				match follow t with
				| TInst({ cl_path = [],"Array" },[t]) -> t
				| TAbstract (a,pl) -> loop (Abstract.get_underlying_type a pl)
				| _ -> t_dynamic
			in
			loop arr.etype
		| _ -> e.etype
	in
	to_type ctx (loop e)

and class_type ?(tref=None) ctx c pl statics =
	let c = if (has_class_flag c CExtern) then resolve_class ctx c pl statics else c in
	let key_path = (if statics then "$" ^ snd c.cl_path else snd c.cl_path) :: fst c.cl_path in
	try
		PMap.find key_path ctx.cached_types
	with Not_found when (has_class_flag c CInterface) && not statics ->
		let vp = {
			vfields = [||];
			vindex = PMap.empty;
		} in
		let t = HVirtual vp in
		ctx.cached_types <- PMap.add key_path t ctx.cached_types;
		let rec loop c =
			let fields = List.fold_left (fun acc (i,_) -> loop i @ acc) [] c.cl_implements in
			PMap.fold (fun cf acc -> cfield_type ctx cf :: acc) c.cl_fields fields
		in
		let fields = loop c in
		vp.vfields <- Array.of_list fields;
		Array.iteri (fun i (n,_,_) -> vp.vindex <- PMap.add n i vp.vindex) vp.vfields;
		t
	| Not_found ->
		let pname = s_type_path (List.tl key_path, List.hd key_path) in
		let p = {
			pname = pname;
			pid = alloc_string ctx pname;
			psuper = None;
			pclassglobal = None;
			pproto = [||];
			pfields = [||];
			pindex = PMap.empty;
			pvirtuals = [||];
			pfunctions = PMap.empty;
			pnfields = -1;
			pinterfaces = PMap.empty;
			pbindings = [];
		} in
		let t = (if Meta.has Meta.Struct c.cl_meta && not statics then HStruct p else HObj p) in
		(match tref with
		| None -> ()
		| Some r -> r := Some t);
		ctx.ct_depth <- ctx.ct_depth + 1;
		ctx.cached_types <- PMap.add key_path t ctx.cached_types;
		if c.cl_path = ([],"Array") then die "" __LOC__;
		if c == ctx.base_class then begin
			if statics then die "" __LOC__;
			p.pnfields <- 1;
		end;
		let tsup = (match c.cl_super with
			| Some (csup,pl) when not statics -> Some (class_type ctx csup [] statics)
			| _ -> if statics then Some (class_type ctx ctx.base_class [] false) else None
		) in
		let start_field, virtuals = (match tsup with
			| None -> 0, [||]
			| Some ((HObj psup | HStruct psup) as pt) ->
				if is_struct t <> is_struct pt then abort (if is_struct t then "Struct cannot extend a not struct class" else "Class cannot extend a struct") c.cl_pos;
				if psup.pnfields < 0 then die "" __LOC__;
				p.psuper <- Some psup;
				psup.pnfields, psup.pvirtuals
			| _ -> die "" __LOC__
		) in
		let fa = DynArray.create() and pa = DynArray.create() and virtuals = DynArray.of_array virtuals in
		let add_field name get_t =
			let fid = DynArray.length fa + start_field in
			let str = alloc_string ctx name in
			p.pindex <- PMap.add name (fid, HVoid) p.pindex;
			DynArray.add fa (name, str, HVoid);
			ctx.ct_delayed <- (fun() ->
				let t = get_t() in
				p.pindex <- PMap.add name (fid, t) p.pindex;
				Array.set p.pfields (fid - start_field) (name, str, t);
			) :: ctx.ct_delayed;
			fid
		in
		List.iter (fun f ->
			if is_extern_field f || (statics && f.cf_name = "__meta__") then () else
			let fid = (match f.cf_kind with
			| Method m when m <> MethDynamic && not statics ->
				let g = alloc_fid ctx c f in
				p.pfunctions <- PMap.add f.cf_name g p.pfunctions;
				let virt = if has_class_field_flag f CfOverride then
					let vid = (try -(fst (get_index f.cf_name p))-1 with Not_found -> die "" __LOC__) in
					DynArray.set virtuals vid g;
					Some vid
				else if is_overridden ctx c f then begin
					let vid = DynArray.length virtuals in
					DynArray.add virtuals g;
					p.pindex <- PMap.add f.cf_name (-vid-1,HVoid) p.pindex;
					Some vid
				end else
					None
				in
				DynArray.add pa { fname = f.cf_name; fid = alloc_string ctx f.cf_name; fmethod = g; fvirtual = virt; };
				None
			| Method MethDynamic when has_class_field_flag f CfOverride ->
				Some (try fst (get_index f.cf_name p) with Not_found -> die "" __LOC__)
			| _ ->
				let fid = add_field f.cf_name (fun() ->
					let t = to_type ctx f.cf_type in
					if has_meta (Meta.Custom ":packed") f.cf_meta then begin
						(match t with HStruct _ -> () | _ -> abort "Packed field should be struct" f.cf_pos);
						HPacked t
					end else t
				) in
				Some fid
			) in
			match f.cf_kind, fid with
			| Method _, Some fid -> p.pbindings <- (fid, alloc_fun_path ctx c.cl_path f.cf_name) :: p.pbindings
			| _ -> ()
		) (if statics then c.cl_ordered_statics else c.cl_ordered_fields);
		if not statics then begin
			(* add interfaces *)
			List.iter (fun (i,pl) ->
				let rid = ref (-1) in
				rid := add_field "" (fun() ->
					let t = to_type ctx (TInst (i,pl)) in
					p.pinterfaces <- PMap.add t !rid p.pinterfaces;
					t
				);
			) c.cl_implements;
			(* check toString *)
			(try
				let cf = PMap.find "toString" c.cl_fields in
				if has_class_field_flag cf CfOverride || PMap.mem "__string" c.cl_fields || not (is_to_string cf.cf_type) then raise Not_found;
				DynArray.add pa { fname = "__string"; fid = alloc_string ctx "__string"; fmethod = alloc_fun_path ctx c.cl_path "__string"; fvirtual = None; }
			with Not_found ->
				());
		end else begin
			(match c.cl_constructor with
			| Some f when not (is_extern_field f) ->
				p.pbindings <- ((try fst (get_index "__constructor__" p) with Not_found -> die "" __LOC__),alloc_fid ctx c f) :: p.pbindings
			| _ -> ());
		end;
		p.pnfields <- DynArray.length fa + start_field;
		p.pfields <- DynArray.to_array fa;
		p.pproto <- DynArray.to_array pa;
		p.pvirtuals <- DynArray.to_array virtuals;
		ctx.ct_depth <- ctx.ct_depth - 1;
		if ctx.ct_depth = 0 then begin
			let todo = ctx.ct_delayed in
			ctx.ct_delayed <- [];
			List.iter (fun f -> f()) todo;
		end;
		if not statics && c != ctx.core_type && c != ctx.core_enum then p.pclassglobal <- Some (fst (class_global ctx (if statics then ctx.base_class else c)));
		t

and enum_type ?(tref=None) ctx e =
	let key_path = snd e.e_path :: fst e.e_path in
	try
		PMap.find key_path ctx.cached_types
	with Not_found ->
		let ename = s_type_path e.e_path in
		let et = {
			eglobal = None;
			ename = ename;
			eid = alloc_string ctx ename;
			efields = [||];
		} in
		let t = HEnum et in
		(match tref with
		| None -> ()
		| Some r -> r := Some t);
		ctx.cached_types <- PMap.add key_path t ctx.cached_types;
		et.efields <- Array.of_list (List.map (fun f ->
			let f = PMap.find f e.e_constrs in
			let args = (match f.ef_type with
				| TFun (args,_) -> Array.of_list (List.map (fun (_,_,t) -> to_type ctx t) args)
				| _ -> [||]
			) in
			(f.ef_name, alloc_string ctx f.ef_name, args)
		) e.e_names);
		let ct = enum_class ctx e in
		et.eglobal <- Some (alloc_global ctx (match ct with HObj o -> o.pname | _ -> die "" __LOC__) ct);
		t

and enum_class ctx e =
	let key_path = ("$" ^ snd e.e_path) :: fst e.e_path in
	try
		PMap.find key_path ctx.cached_types
	with Not_found ->
		let pname = s_type_path (List.tl key_path, List.hd key_path) in
		let p = {
			pname = pname;
			pid = alloc_string ctx pname;
			psuper = None;
			pclassglobal = None;
			pproto = [||];
			pfields = [||];
			pindex = PMap.empty;
			pvirtuals = [||];
			pfunctions = PMap.empty;
			pnfields = -1;
			pinterfaces = PMap.empty;
			pbindings = [];
		} in
		let t = HObj p in
		ctx.cached_types <- PMap.add key_path t ctx.cached_types;
		p.psuper <- Some (match class_type ctx ctx.base_enum [] false with HObj o -> o | _ -> die "" __LOC__);
		t

and alloc_fun_path ctx path name =
	lookup ctx.cfids (name, path) (fun() -> ())

and alloc_fid ctx c f =
	match f.cf_kind with
	| Var _ -> die "" __LOC__
	| _ -> alloc_fun_path ctx c.cl_path f.cf_name

and alloc_eid ctx e f =
	alloc_fun_path ctx e.e_path f.ef_name

and alloc_function_name ctx f =
	alloc_fun_path ctx ([],"") f

and alloc_global ctx name t =
	lookup ctx.cglobals name (fun() -> t)

and class_global ?(resolve=true) ctx c =
	let static = c != ctx.base_class in
	let c = if resolve && is_array_type (HObj { null_proto with pname = s_type_path c.cl_path }) then ctx.array_impl.abase else c in
	let c = resolve_class ctx c (extract_param_types c.cl_params) static in
	let t = class_type ctx c [] static in
	alloc_global ctx ("$" ^ s_type_path c.cl_path) t, t

let resolve_class_global ctx cpath =
	lookup ctx.cglobals ("$" ^ cpath) (fun() -> die "" __LOC__)

let resolve_type ctx path =
	PMap.find path ctx.cached_types

let alloc_std ctx name args ret =
	let lib = "std" in
	(* different from :hlNative to prevent mismatch *)
	let nid = lookup ctx.cnatives ("$" ^ name ^ "@" ^ lib, -1) (fun() ->
		let fid = alloc_fun_path ctx ([],"std") name in
		Hashtbl.add ctx.defined_funs fid ();
		(alloc_string ctx lib, alloc_string ctx name,HFun (args,ret),fid)
	) in
	let _,_,_,fid = DynArray.get ctx.cnatives.arr nid in
	fid

let alloc_fresh ctx t =
	let rid = DynArray.length ctx.m.mregs.arr in
	DynArray.add ctx.m.mregs.arr t;
	rid

let alloc_tmp ctx t =
	if not ctx.optimize then alloc_fresh ctx t else
	let a = try PMap.find t ctx.m.mallocs with Not_found ->
		let a = {
			a_all = [];
			a_hold = [];
		} in
		ctx.m.mallocs <- PMap.add t a ctx.m.mallocs;
		a
	in
	match a.a_all with
	| [] ->
		let r = alloc_fresh ctx t in
		a.a_all <- [r];
		r
	| r :: _ ->
		r

let current_pos ctx =
	DynArray.length ctx.m.mops

let rtype ctx r =
	DynArray.get ctx.m.mregs.arr r

let hold ctx r =
	if not ctx.optimize then () else
	let t = rtype ctx r in
	let a = PMap.find t ctx.m.mallocs in
	let rec loop l =
		match l with
		| [] -> if List.mem r a.a_hold then [] else die "" __LOC__
		| n :: l when n = r -> l
		| n :: l -> n :: loop l
	in
	a.a_all <- loop a.a_all;
	a.a_hold <- r :: a.a_hold

let free ctx r =
	if not ctx.optimize then () else
	let t = rtype ctx r in
	let a = PMap.find t ctx.m.mallocs in
	let last = ref true in
	let rec loop l =
		match l with
		| [] -> die "" __LOC__
		| n :: l when n = r ->
			if List.mem r l then last := false;
			l
		| n :: l -> n :: loop l
	in
	a.a_hold <- loop a.a_hold;
	(* insert sorted *)
	let rec loop l =
		match l with
		| [] -> [r]
		| n :: _ when n > r -> r :: l
		| n :: l -> n :: loop l
	in
	if !last then a.a_all <- loop a.a_all

let decl_var ctx v =
	ctx.m.mdeclared <- v.v_id :: ctx.m.mdeclared

let alloc_var ctx v new_var =
	if new_var then decl_var ctx v;
	try
		Hashtbl.find ctx.m.mvars v.v_id
	with Not_found ->
		let r = alloc_tmp ctx (to_type ctx v.v_type) in
		hold ctx r;
		Hashtbl.add ctx.m.mvars v.v_id r;
		r


let push_op ctx o =
	DynArray.add ctx.m.mdebug ctx.m.mcurpos;
	DynArray.add ctx.m.mops o

let op ctx o =
	match o with
	| OMov (a,b) when a = b ->
		()
	| _ ->
		push_op ctx o

let set_op ctx pos o =
	DynArray.set ctx.m.mops pos o

let jump ctx f =
	let pos = current_pos ctx in
	op ctx (OJAlways (-1)); (* loop *)
	(fun() -> set_op ctx pos (f (current_pos ctx - pos - 1)))

let jump_back ctx =
	let pos = current_pos ctx in
	op ctx (OLabel 0);
	(fun() -> op ctx (OJAlways (pos - current_pos ctx - 1)))

let reg_int ctx v =
	let r = alloc_tmp ctx HI32 in
	op ctx (OInt (r,alloc_i32 ctx (Int32.of_int v)));
	r

let shl ctx idx v =
	if v = 0 then
		idx
	else begin
		hold ctx idx;
		let rv = reg_int ctx v in
		let idx2 = alloc_tmp ctx HI32 in
		op ctx (OShl (idx2, idx, rv));
		free ctx idx;
		idx2;
	end

let set_default ctx r =
	match rtype ctx r with
	| HUI8 | HUI16 | HI32 | HI64 ->
		op ctx (OInt (r,alloc_i32 ctx 0l))
	| HF32 | HF64 ->
		op ctx (OFloat (r,alloc_float ctx 0.))
	| HBool ->
		op ctx (OBool (r, false))
	| HType ->
		op ctx (OType (r, HVoid))
	| _ ->
		op ctx (ONull r)

let read_mem ctx rdst bytes index t =
	match t with
	| HUI8 ->
		op ctx (OGetUI8 (rdst,bytes,index))
	| HUI16 ->
		op ctx (OGetUI16 (rdst,bytes,index))
	| HI32 | HI64 | HF32 | HF64 ->
		op ctx (OGetMem (rdst,bytes,index))
	| _ ->
		die "" __LOC__

let write_mem ctx bytes index t r =
	match t with
	| HUI8 ->
		op ctx (OSetUI8 (bytes,index,r))
	| HUI16 ->
		op ctx (OSetUI16 (bytes,index,r))
	| HI32 | HI64 | HF32 | HF64 ->
		op ctx (OSetMem (bytes,index,r))
	| _ ->
		die "" __LOC__

let common_type ctx e1 e2 for_eq p =
	let t1 = to_type ctx e1.etype in
	let t2 = to_type ctx e2.etype in
	let rec loop t1 t2 =
		if t1 == t2 then t1 else
		match t1, t2 with
		| HUI8, (HUI16 | HI32 | HI64 | HF32 | HF64) -> t2
		| HUI16, (HI32 | HI64 | HF32 | HF64) -> t2
		| (HI32 | HI64), HF32 -> t2 (* possible loss of precision *)
		| (HI32 | HI64 | HF32), HF64 -> t2
		| (HUI8|HUI16|HI32|HI64|HF32|HF64), (HUI8|HUI16|HI32|HI64|HF32|HF64) -> t1
		| (HUI8|HUI16|HI32|HI64|HF32|HF64), (HNull t2) -> if for_eq then HNull (loop t1 t2) else loop t1 t2
		| (HNull t1), (HUI8|HUI16|HI32|HI64|HF32|HF64) -> if for_eq then HNull (loop t1 t2) else loop t1 t2
		| (HNull t1), (HNull t2) -> if for_eq then HNull (loop t1 t2) else loop t1 t2
		| HDyn, (HUI8|HUI16|HI32|HI64|HF32|HF64) -> HF64
		| (HUI8|HUI16|HI32|HI64|HF32|HF64), HDyn -> HF64
		| HDyn, _ -> HDyn
		| _, HDyn -> HDyn
		| _ when for_eq && safe_cast t1 t2 -> t2
		| _ when for_eq && safe_cast t2 t1 -> t1
		| HBool, HNull HBool when for_eq -> t2
		| HNull HBool, HBool when for_eq -> t1
		| HObj _, HVirtual _ | HVirtual _, HObj _ | HVirtual _ , HVirtual _ -> HDyn
		| HFun _, HFun _ -> HDyn
		| _ ->
			abort ("Don't know how to compare " ^ tstr t1 ^ " and " ^ tstr t2) p
	in
	loop t1 t2

let captured_index ctx v =
	if not (has_var_flag v VCaptured) then None else try Some (PMap.find v.v_id ctx.m.mcaptured.c_map) with Not_found -> None

let real_name v =
	let rec loop = function
		| [] -> v.v_name
		| (Meta.RealPath,[EConst (String(name,_)),_],_) :: _ -> name
		| _ :: l -> loop l
	in
	match loop v.v_meta with
	| "_gthis" -> "this"
	| name -> name

let is_gen_local ctx v = match v.v_kind with
	| VUser _ -> false
	| _ -> true

let add_assign ctx v =
	if is_gen_local ctx v then () else
	let name = real_name v in
	ctx.m.massign <- (alloc_string ctx name, current_pos ctx - 1) :: ctx.m.massign

let add_capture ctx r =
	Array.iter (fun v ->
		let name = real_name v in
		ctx.m.massign <- (alloc_string ctx name, -(r+2)) :: ctx.m.massign
	) ctx.m.mcaptured.c_vars

let before_return ctx =
	let rec loop i =
		if i > 0 then begin
			op ctx (OEndTrap false);
			loop (i - 1)
		end
	in
	loop ctx.m.mtrys

let before_break_continue ctx =
	let rec loop i =
		if i > 0 then begin
			op ctx (OEndTrap false);
			loop (i - 1)
		end
	in
	loop (ctx.m.mtrys - ctx.m.mloop_trys)

let type_value ctx t p =
	match t with
	| TClassDecl c ->
		let g, t = class_global ctx c in
		let r = alloc_tmp ctx t in
		op ctx (OGetGlobal (r, g));
		r
	| TAbstractDecl a ->
		let r = alloc_tmp ctx (class_type ctx ctx.base_type [] false) in
		(match a.a_path with
		| [], "Int" -> op ctx (OGetGlobal (r, alloc_global ctx "$Int" (rtype ctx r)))
		| [], "Float" -> op ctx (OGetGlobal (r, alloc_global ctx "$Float" (rtype ctx r)))
		| [], "Bool" -> op ctx (OGetGlobal (r, alloc_global ctx "$Bool" (rtype ctx r)))
		| [], "Class" -> op ctx (OGetGlobal (r, fst (class_global ctx ctx.base_class)))
		| [], "Enum" -> op ctx (OGetGlobal (r, fst (class_global ctx ctx.base_enum)))
		| [], "Dynamic" -> op ctx (OGetGlobal (r, alloc_global ctx "$Dynamic" (rtype ctx r)))
		| _ -> abort ("Unsupported type value " ^ s_type_path (t_path t)) p);
		r
	| TEnumDecl e ->
		let r = alloc_tmp ctx (enum_class ctx e) in
		let rt = rtype ctx r in
		op ctx (OGetGlobal (r, alloc_global ctx (match rt with HObj o -> o.pname | _ -> die "" __LOC__) rt));
		r
	| TTypeDecl _ ->
		die "" __LOC__

let rec eval_to ctx e (t:ttype) =
	match e.eexpr, t with
	| TConst (TInt i), HF64 ->
		let r = alloc_tmp ctx t in
		op ctx (OFloat (r,alloc_float ctx (Int32.to_float i)));
		r
	(* this causes a bug with NG, to be reviewed later
	| TConst (TInt i), HF32 ->
		let r = alloc_tmp ctx t in
		let bits = Int32.bits_of_float (Int32.to_float i) in
		op ctx (OFloat (r,alloc_float ctx (Int64.float_of_bits (Int64.of_int32 bits))));
		r
	| TConst (TFloat f), HF32 ->
		let r = alloc_tmp ctx t in
		let bits = Int32.bits_of_float (float_of_string f) in
		op ctx (OFloat (r,alloc_float ctx (Int64.float_of_bits (Int64.of_int32 bits))));
		r
	*)
	| _ ->
		let r = eval_expr ctx e in
		cast_to ctx r t e.epos

and to_string ctx (r:reg) p =
	let rt = rtype ctx r in
	if safe_cast rt ctx.tstring then r else
	match rt with
	| HUI8 | HUI16 | HI32 ->
		let len = alloc_tmp ctx HI32 in
		hold ctx len;
		let lref = alloc_tmp ctx (HRef HI32) in
		let bytes = alloc_tmp ctx HBytes in
		op ctx (ORef (lref,len));
		op ctx (OCall2 (bytes,alloc_std ctx "itos" [HI32;HRef HI32] HBytes,cast_to ctx r HI32 p,lref));
		let out = alloc_tmp ctx ctx.tstring in
		op ctx (OCall2 (out,alloc_fun_path ctx ([],"String") "__alloc__",bytes,len));
		free ctx len;
		out
	| HF32 | HF64 ->
		let len = alloc_tmp ctx HI32 in
		let lref = alloc_tmp ctx (HRef HI32) in
		let bytes = alloc_tmp ctx HBytes in
		op ctx (ORef (lref,len));
		op ctx (OCall2 (bytes,alloc_std ctx "ftos" [HF64;HRef HI32] HBytes,cast_to ctx r HF64 p,lref));
		let out = alloc_tmp ctx ctx.tstring in
		op ctx (OCall2 (out,alloc_fun_path ctx ([],"String") "__alloc__",bytes,len));
		out
	| _ ->
		let r = cast_to ctx r HDyn p in
		let out = alloc_tmp ctx ctx.tstring in
		op ctx (OJNotNull (r,2));
		op ctx (ONull out);
		op ctx (OJAlways 1);
		op ctx (OCall1 (out,alloc_fun_path ctx ([],"Std") "string",r));
		out

and cast_to ?(force=false) ctx (r:reg) (t:ttype) p =
	let rt = rtype ctx r in
	if safe_cast rt t then r else
	match rt, t with
	| _, HVoid ->
		alloc_tmp ctx HVoid
	| HVirtual _, HVirtual _ ->
		let tmp = alloc_tmp ctx HDyn in
		op ctx (OMov (tmp,r));
		cast_to ctx tmp t p
	| (HUI8 | HUI16 | HI32 | HI64 | HF32 | HF64), (HF32 | HF64) ->
		let tmp = alloc_tmp ctx t in
		op ctx (OToSFloat (tmp, r));
		tmp
	| (HUI8 | HUI16 | HI32 | HI64 | HF32 | HF64), (HUI8 | HUI16 | HI32 | HI64) ->
		let tmp = alloc_tmp ctx t in
		op ctx (OToInt (tmp, r));
		tmp
	| HObj o, HVirtual _ ->
		let out = alloc_tmp ctx t in
		(try
			let rec lookup_intf o =
				try
					PMap.find t o.pinterfaces
				with Not_found ->
					match o.psuper with
					| None -> raise Not_found
					| Some o -> lookup_intf o
			in
			let fid = lookup_intf o in
			(* memoisation *)
			let need_null_check r =
				not (r = 0 && ctx.m.mhasthis)
			in
			let jend = if need_null_check r then
				let jnull = jump ctx (fun d -> OJNotNull (r,d)) in
				op ctx (ONull out);
				let jend = jump ctx (fun d -> OJAlways d) in
				jnull();
				jend
			else
				(fun() -> ())
			in
			op ctx (OField (out, r, fid));
			let j = jump ctx (fun d -> OJNotNull (out,d)) in
			op ctx (OToVirtual (out,r));
			op ctx (OSetField (r, fid, out));
			jend();
			j();
		with Not_found ->
			(* not an interface *)
			op ctx (OToVirtual (out,r)));
		out
	| (HDynObj | HDyn) , HVirtual _ ->
		let out = alloc_tmp ctx t in
		op ctx (OToVirtual (out,r));
		out
	| HDyn, _ ->
		let out = alloc_tmp ctx t in
		op ctx (OSafeCast (out, r));
		out
	| HNull rt, _ when t = rt ->
		let out = alloc_tmp ctx t in
		op ctx (OSafeCast (out, r));
		out
	| HVoid, HDyn ->
		let tmp = alloc_tmp ctx HDyn in
		op ctx (ONull tmp);
		tmp
	| _ , HDyn ->
		let tmp = alloc_tmp ctx HDyn in
		op ctx (OToDyn (tmp, r));
		tmp
	| _, HNull t when rt == t ->
		let tmp = alloc_tmp ctx (HNull t) in
		op ctx (OToDyn (tmp, r));
		tmp
	| HNull t1, HNull t2 ->
		let j = jump ctx (fun n -> OJNull (r,n)) in
		let rtmp = alloc_tmp ctx t1 in
		op ctx (OSafeCast (rtmp,r));
		let out = cast_to ctx rtmp t p in
		op ctx (OJAlways 1);
		j();
		op ctx (ONull out);
		out
	| HRef t1, HNull t2 ->
		let j = jump ctx (fun n -> OJNull (r,n)) in
		let rtmp = alloc_tmp ctx t1 in
		op ctx (OUnref (rtmp,r));
		let out = cast_to ctx rtmp t p in
		op ctx (OJAlways 1);
		j();
		op ctx (ONull out);
		out
	| (HUI8 | HUI16 | HI32 | HI64 | HF32 | HF64), HNull ((HF32 | HF64) as t) ->
		let tmp = alloc_tmp ctx t in
		op ctx (OToSFloat (tmp, r));
		let r = alloc_tmp ctx (HNull t) in
		op ctx (OToDyn (r,tmp));
		r
	| (HUI8 | HUI16 | HI32 | HI64 | HF32 | HF64), HNull ((HUI8 | HUI16 | HI32) as t) ->
		let tmp = alloc_tmp ctx t in
		op ctx (OToInt (tmp, r));
		let r = alloc_tmp ctx (HNull t) in
		op ctx (OToDyn (r,tmp));
		r
	| HNull ((HUI8 | HUI16 | HI32 | HI64) as it), (HF32 | HF64) ->
		let i = alloc_tmp ctx it in
		op ctx (OSafeCast (i,r));
		let tmp = alloc_tmp ctx t in
		op ctx (OToSFloat (tmp, i));
		tmp
	| HNull ((HF32 | HF64) as it), (HUI8 | HUI16 | HI32 | HI64) ->
		let i = alloc_tmp ctx it in
		op ctx (OSafeCast (i,r));
		let tmp = alloc_tmp ctx t in
		op ctx (OToInt (tmp, i));
		tmp
	| HFun (args1,ret1), HFun (args2, ret2) when List.length args1 = List.length args2 ->
		let fid = gen_method_wrapper ctx rt t p in
		let fr = alloc_tmp ctx t in
		op ctx (OJNotNull (r,2));
		op ctx (ONull fr);
		op ctx (OJAlways 1);
		op ctx (OInstanceClosure (fr,fid,r));
		fr
	| HObj _, HObj _ when is_array_type rt && is_array_type t ->
		let out = alloc_tmp ctx t in
		op ctx (OSafeCast (out, r));
		out
	| HNull _, HRef t2 ->
		let out = alloc_tmp ctx t in
		op ctx (OJNotNull (r,2));
		op ctx (ONull out);
		let j = jump ctx (fun n -> OJAlways n) in
		let r = cast_to ctx r t2 p in
		let r2 = alloc_tmp ctx t2 in
		op ctx (OMov (r2, r));
		hold ctx r2; (* retain *)
		op ctx (ORef (out,r2));
		j();
		out
	| _, HRef t2 ->
		let r = cast_to ctx r t2 p in
		let r2 = alloc_tmp ctx t2 in
		op ctx (OMov (r2, r));
		hold ctx r2; (* retain *)
		let out = alloc_tmp ctx t in
		op ctx (ORef (out,r2));
		out
	| _ ->
		if force then
			let out = alloc_tmp ctx t in
			op ctx (OSafeCast (out, r));
			out
		else
			abort ("Don't know how to cast " ^ tstr rt ^ " to " ^ tstr t) p

and unsafe_cast_to ?(debugchk=true) ctx (r:reg) (t:ttype) p =
	let rt = rtype ctx r in
	if safe_cast rt t then
		r
	else
	match rt with
	| HFun _ ->
		cast_to ctx r t p
	| HDyn when is_array_type t ->
		cast_to ctx r t p
	| (HDyn | HObj _) when (match t with HVirtual _ -> true | _ -> false) ->
		cast_to ctx r t p
	| HObj _ when is_array_type rt && is_array_type t ->
		cast_to ctx r t p
	| HVirtual _ when (match t with HObj _ | HVirtual _ -> true | _ -> false) ->
		cast_to ~force:true ctx r t p
	| _ ->
		if is_dynamic (rtype ctx r) && is_dynamic t then
			let r2 = alloc_tmp ctx t in
			op ctx (OUnsafeCast (r2,r));
			if ctx.com.debug && debugchk then begin
				hold ctx r2;
				let r3 = cast_to ~force:true ctx r t p in
				let j = jump ctx (fun n -> OJEq (r2,r3,n)) in
				op ctx (OAssert 0);
				j();
				free ctx r2;
			end;
			r2
		else
			cast_to ~force:true ctx r t p

and object_access ctx eobj t f =
	match t with
	| HObj p | HStruct p ->
		(try
			let fid = fst (get_index f.cf_name p) in
			if f.cf_kind = Method MethNormal then
				AInstanceProto (eobj, -fid-1)
			else
				AInstanceField (eobj, fid)
		with Not_found ->
			ADynamic (eobj, alloc_string ctx f.cf_name))
	| HVirtual v ->
		(try
			let fid = PMap.find f.cf_name v.vindex in
			if f.cf_kind = Method MethNormal then
				AVirtualMethod (eobj, fid)
			else
				AInstanceField (eobj, fid)
		with Not_found ->
			ADynamic (eobj, alloc_string ctx f.cf_name))
	| HDyn ->
		ADynamic (eobj, alloc_string ctx f.cf_name)
	| _ ->
		abort ("Unsupported field access " ^ tstr t) eobj.epos

and direct_method_call ctx c f ethis =
	if (match f.cf_kind with Method m -> m = MethDynamic | Var _ -> true) then
		false
	else if (has_class_flag c CInterface) then
		false
	else if (match c.cl_kind with KTypeParameter _ ->  true | _ -> false) then
		false
	else if is_overridden ctx c f && ethis.eexpr <> TConst(TSuper) then
		false
	else
		true

and get_access ctx e =
	match e.eexpr with
	| TField (ethis, a) ->
		(match a, follow ethis.etype with
		| FStatic (c,({ cf_