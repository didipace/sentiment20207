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
open Extlib_leftovers
open IO
open IO.BigEndian
open JvmGlobals
open JvmSignature

(* High-level constant pool *)

let utf8jvm (input : string) : bytes =
	let channel = IO.output_bytes () in
	UTF8.iter (fun c ->
		let code = UCharExt.code c in
		match code with
			| b when (b > 0 && b <= 0x7F) ->
			IO.write_byte channel b
			(* includes null byte: *)
			| b when (b <= 0x7FF) ->
			IO.write_byte channel (0xC0 lor ((b lsr  6)          ));
			IO.write_byte channel (0x80 lor ((b       ) land 0x3F))
			| b when (b <= 0xFFFF) ->
			IO.write_byte channel (0xE0 lor ((b lsr 12)          ));
			IO.write_byte channel (0x80 lor ((b lsr  6) land 0x3F));
			IO.write_byte channel (0x80 lor ((b       ) land 0x3F))
			| b ->
			IO.write_byte channel 0xED;
			IO.write_byte channel (0xA0 lor ((b lsr 16) - 1      ));
			IO.write_byte channel (0x80 lor ((b lsr 10) land 0x3F));
			IO.write_byte channel 0xED;
			IO.write_byte channel (0xB0 lor ((b lsr  6) land 0x0F));
			IO.write_byte channel (0x80 lor ((b       ) land 0x3F))
	) input;
	IO.close_out channel
;;

class constant_pool = object(self)
	val pool = DynArray.create ();
	val lut = Hashtbl.create 0;
	val luti = Hashtbl.create 0;
	val mutable next_index = 1;
	val mutable closed = false
	val inner_classes = Hashtbl.create 0

	method add const =
		try
			Hashtbl.find lut const
		with Not_found ->
			assert (not closed);
			let i = next_index in
			next_index <- next_index + 1;
			DynArray.add pool const;
			Hashtbl.add lut const i;
			Hashtbl.add luti i (DynArray.length pool - 1);
			match const with
			| ConstDouble _ | ConstLong _ ->
				next_index <- next_index + 1;
				i
			| _ ->
				i

	method get i =
		DynArray.get pool (Hashtbl.find luti i)

	method private s_type_path (p,s) = match p with [] -> s | _ -> String.concat "/" p ^ "/" ^ s

	method add_type s =
		let offset = self#add (ConstUtf8 s) in
		self#add (ConstClass offset);

	method add_path path =
		let s = self#s_type_path path in
		let offset = self#add_type s in
		if String.contains (snd path) '$' && not (ExtString.String.starts_with s "[") then begin
			let name1,name2 = ExtString.String.split (snd path) "$" in
			Hashtbl.replace inner_classes ((fst path,name1),n