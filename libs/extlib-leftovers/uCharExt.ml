(* 
 * UChar - Unicode (ISO-UCS) characters
 * Copyright (C) 2002, 2003 Yamagata Yoriyuki
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version,
 * with the special exception on linking described in file LICENSE.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA
 *)

type t = int

exception Out_of_range

external unsafe_chr_of_uint : int -> t = "%identity"
external uint_code : t -> int = "%identity"

let char_of c = 
  if c >= 0 && c < 0x100 then Char.chr c else raise Out_of_range

let of_char = Char.code

let code c = if c >= 0 then c else raise Out_of_range

let chr n =
  if n >= 0 && n lsr 31 = 0 then n else invalid_arg "UChar.chr"
