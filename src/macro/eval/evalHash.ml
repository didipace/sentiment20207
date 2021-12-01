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

let reverse_map = Hashtbl.create 0

let rev_hash i = Hashtbl.find reverse_map i

let hash f =
	let i = Hashtbl.hash f in
	Hashtbl.replace reverse_map i f;
	i

let path_hash path = hash (Globals.s_type_path path)

let key_length = hash "length"
let key_toString = hash "toString"
let key_OutsideBounds = hash "OutsideBounds"
let key_low = hash "low"
let key_high = hash "high"
let key_next = hash "next"
let key_hasNext = hash "hasNext"
let key___meta__ = hash "__meta__"
let key