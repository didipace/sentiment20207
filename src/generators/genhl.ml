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
				| Non