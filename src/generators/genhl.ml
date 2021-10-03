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
	| "hl.types.ArrayDyn" | "hl.types.ArrayBytes_Int" | "hl.types.ArrayBytes_Float" | "hl.types.ArrayObj" | "hl.types.ArrayBytes