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
open EvalValue
open EvalContext
open EvalHash

let no_field =
	vnull

let proto_field_direct proto name =
	proto.pfields.(get_proto_field_index_raise proto name)

let rec proto_field_raise proto name =
	try proto_field_direct proto name
	with Not_found -> match proto.pparent with
		| Some proto -> proto_field_raise proto name
		| _ -> raise Not_found

let instance_field vi name =
	vi.ifields.(get_instan