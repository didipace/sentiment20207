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
		mat