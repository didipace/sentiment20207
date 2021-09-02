
(*
	The Haxe Compiler
	Copyright (C) 2005-2019  Haxe Foundation

	This program is free software; you can redistribute it and/or
	modify it under the terms of the GNU General Public License
	as published by the Free Software Foundation; either version 2
	of the License, or (at your option) any later version.

	This program is distributed in the hope that it will be useful,
	but WITHOUT ANY WARRANTY; without even the implied warranty of
	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
	GNU General Public License for more details.

	You should have received a copy of the GNU General Public License
	along with this program; if not, write to the Free Software
	Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
 *)

open Globals
open EvalHash
open EvalValue
open EvalEncode
open EvalDecode
open EvalContext
open EvalPrinting
open EvalExceptions
open EvalField
open EvalMisc

(* Helper *)

let unexpected_value_p v s p =
	let str = match v with
		| VNull -> "Null Access"
		| _ -> Printf.sprintf "Unexpected value %s, expected %s" (value_string v) s
	in
	throw_string str p

let as_array p = function
	| VArray va -> va
	| v -> unexpected_value_p v "Array" p

let as_bytes p = function
	| VInstance {ikind = IBytes s} -> s
	| v -> unexpected_value_p v "Bytes" p

let as_enum_value p = function
	| VEnumValue ve -> ve
	| v -> unexpected_value_p v "enum value" p

let as_int p = function
	| VInt32 i -> Int32.to_int i
	| VFloat f -> int_of_float f
	| v -> unexpected_value_p v "int" p

let as_vector p = function
	| VVector vv -> vv
	| v -> unexpected_value_p v "Vector" p

let cannot_call v p =
	throw (EvalString.create_unknown ("Cannot call " ^ (value_string v))) p

let decode_int_p v p = match v with
	| VInt32 i -> Int32.to_int i
	| VFloat f -> int_of_float f
	| _ -> unexpected_value_p v "int" p

let check_stack_depth env =
	if env.env_stack_depth > (get_ctx()).max_stack_depth then
		exc_string "Stack overflow"

(* Emitter *)

let apply env exec =
	exec env

(* Objects and values *)

let emit_null _ = vnull

let emit_local_declaration i exec env =
	env.env_locals.(i) <- exec env;
	vnull

let emit_capture_declaration i exec env =
	env.env_captures.(i) <- exec env;
	vnull

let emit_const v _ = v

let emit_null_check exec p env = match exec env with
	| VNull -> throw_string "Null Access" p
	| v -> v

let emit_new_array env =
	encode_array_instance (EvalArray.create [||])

let emit_new_vector_int i p env =
	if i < 0 then exc_string_p "Vector size must be >= 0" p;
	let a = try
		Array.make i vnull
	with Invalid_argument _ ->
		exc_string_p (Printf.sprintf "Not enough memory to allocate Vector of size %i" i) p;
	in
	encode_vector_instance a

let emit_new_vector exec p env =
	let i = decode_int_p (exec env) p in
	emit_new_vector_int i p env

let emit_special_instance f execs env =
	let vl = List.map (apply env) execs in
	f vl

let emit_object_declaration proto fa env =
	let a = Array.make (Array.length fa) vnull in
	Array.iter (fun (i,exec) -> a.(i) <- exec env) fa;
	vobject {
		ofields = a;
		oproto = OProto proto;
	}

let emit_array_declaration execs env =
	let vl = Array.map (apply env) execs in
	encode_array_instance (EvalArray.create vl)

let emit_type_expr proto env = proto

let emit_mk_pos exec1 exec2 exec3 env =
	let file = exec1 env in
	let min = exec2 env in
	let max = exec3 env in
	encode_pos { pfile = decode_string file; pmin = decode_int min; pmax = decode_int max }

let emit_enum_construction key i execs p env =
	encode_enum_value key i (Array.map (apply env) execs) p

(* Branching *)

let emit_if exec_cond exec_then exec_else env =
	match exec_cond env with
	| VTrue -> exec_then env
	| _ -> exec_else env

let emit_switch exec execs patterns exec_def env =
	let v1 = exec env in
	let rec loop v1 i =
		if i >= Array.length patterns then exec_def env
		else if List.exists (fun exec -> equals v1 (exec env)) patterns.(i) then
			execs.(i) env
		else
			loop v1 (i + 1)
	in
	loop v1 0

let emit_int_switch_map exec cases exec_def p env = match exec env with
	| VInt32 i32 ->
		let i = Int32.to_int i32 in
		begin try
			(IntMap.find i cases) env