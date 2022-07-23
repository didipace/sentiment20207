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

(*  Taken from OCaml source typing/oprint.ml

	This is a better version of string_of_float which prints without loss of precision
	so that float_of_string (float_repres x) = x for all floats x
*)
let valid_float_lexeme s =
	let l = String.length s in
	let rec loop i =
		if i >= l then s ^ "." else
		match s.[i] with
		| '0' .. '9' | '-' -> loop (i+1)
		| _ -> s
	in loop 0

let float_repres f =
	match classify_float f with
	| FP_nan -> "nan"
	| FP_infinite ->
		if f < 0.0 then "neg_infinity" else "infinity"
	| _ ->
		let float_val =
			let s