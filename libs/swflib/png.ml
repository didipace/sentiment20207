(*
 *  PNG File Format Library
 *  Copyright (c)2005 Nicolas Cannasse
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA
 *)

type grey_bits =
	| GBits1
	| GBits2
	| GBits4
	| GBits8
	| GBits16

type grey_alpha_bits =
	| GABits8
	| GABits16

type true_bits =
	| TBits8
	| TBits16

type index_bits =
	| IBits1
	| IBits2
	| IBits4
	| IBits8

type alpha =
	| NoAlpha
	| HaveAlpha

type color =
	| ClGreyScale of grey_bits
	| ClGreyAlpha of grey_alpha_bits
	| ClTrueColor of true_bits * alpha
	| ClIndexed of index_bits

type header = {
	png_width : int;
	png_height : int;
	png_color : color;
	png_interlace : bool;
}

type chunk_id = string

type chunk =
	| CEnd
	| CHeader of header
	| CData of string
	| CPalette of string
	| CUnknown of chunk_id * string

type png = chunk list

type error_msg =
	| Invalid_header
	| Invalid_file
	| Truncated_file
	| Invalid_CRC
	| Invalid_colors
	| Unsupported_colors
	| Invalid_datasize
	| Invalid_filter of int
	| Invalid_array

exception Error of error_msg

let error_msg = function
	| Invalid_header -> "Invalid header"
	| Invalid_file -> "Invalid file"
	| Truncated_file -> "Truncated file"
	| Invalid_CRC -> "Invalid CRC"
	| Invalid_colors -> "Invalid color model"
	| Unsupported_colors -> "Unsupported color model"
	| Invalid_datasize -> "Invalid data size"
	| Invalid_filter f -> "Invalid filter " ^ string_of_int f
	| Invalid_array -> "Invalid array"

let error msg = raise (Error msg)

let is_upper c = ((int_of_char c) land 32) <> 0

let is_critical id = is_upper id.[0]

let is_public id = is_upper id.[1]

let is_reseverd id = is_upper id.[2]

let is_safe_to_copy id = is_upper id.[3]

let is_id_char c =
	(c >= '\065' && c <= '\090') || (c >= '\097' && c <= '\122')

let rec header = function
	| [] -> error Invalid_file
	| CHeader h :: _ -> h
	| _ :: l -> header l

let data f =
	let rec loop acc = function
		| [] ->
			(match List.rev acc with
			| [] -> error Invalid_file
			| l -> String.concat "" l)
		| CData s :: l -> loop (s :: acc) l
		| _ :: l -> loop acc l
	in
	loop [] f

let color_bits = function
	| ClGreyScale g -> (match g with
		| GBits1 -> 1
		| GBits2 -> 2
		| GBits4 -> 4
		| GBits8 -> 8
		| GBits16 -> 16)
	| ClGreyAlpha g -> (match g with
		| GABits8 -> 8
		| GABits16 -> 16)
	| ClTrueColor (t,_) -> (match t with
		| TBits8 -> 8
		| TBits16 -> 16)
	| ClIndexed i -> (match i with
		| IBits1 -> 1
		| IBits2 -> 2
		| IBits4 -> 4
		| IBits8 -> 8)

let crc_table = Array.init 256 (fun n ->
	let c = ref (Int32.of_int n) in
	for k = 0 to 7 do
		if Int32.logand !c 1l <> 0l then
			c := Int32.logxor 0xEDB88320l (Int32.shift_right_logical !c 1)
		else
			c := (Int32.shift_right_logical !c 1);
	done;
	!c)

let input_crc ch =
	let crc = ref 0xFFFFFFFFl in
	let update c =
		let c = Int32.of_int (int_of_char c) in
		let k = Array.unsafe_get crc_table (Int32.to_int (Int32.logand (Int32.logxor !crc c) 0xFFl)) in
		crc := Int32.logxor k (Int32.shift_right_logical !crc 8)
	in
	let ch2 = IO.create_in
		~read:(fun () ->
			let c = IO.read ch in
			update c;
			c
		)
		~input:(fun s p l ->
			let l = IO.input ch s p l in
			for i = 0 to l - 1 do
				update (Bytes.get s (p+i))
			done;
			l
		)
		~close:(fun () ->
			IO.close_in ch
		)
	in
	ch2 , (fun () -> Int32.logxor !crc 0xFFFFFFFFl)

let output_crc ch =
	let crc = ref 0xFFFFFFFFl in
	let update c =
		let c = Int32.of_int (int_of_char c) in
		let k = Array.unsafe_get crc_table (Int32.to_int (Int32.logand (Int32.logxor !crc c) 0xFFl)) in
		crc := Int32.logxor k (Int32.shift_right_logical !crc 8)
	in
	let ch2 = IO.create_out
		~write:(fun c ->
			IO.write ch c;
			update c;
		)
		~output:(fun s p l ->
			let l = IO.output ch s p l in
			for i = 0 to l - 1 do
				update (Bytes.get s (p+i))
			done;
			l
		)
		~flush:(fun () ->
			IO.flush ch
		)
		~close:(fun () ->
			IO.close_out ch
		)
	in
	ch2 , (fun () -> Int32.logxor !crc 0xFFFFFFFFl)

let parse_header ch =
	let width = IO.BigEndian.read_i32 ch in
	let height = IO.BigEndian.read_i32 ch in
	if width < 0 || height < 0 then error Invalid_header