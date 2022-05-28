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

open JvmGlobals
open JvmData

(* Low-level writing corresponding to jvmData. *)

let write_jvm_attribute ch jvma =
	write_ui16 ch jvma.attr_index;
	write_ui32 ch (Bytes.length jvma.attr_data);
	write_bytes ch jvma.attr_data

let write_jvm_attributes ch jvmal =
	write_array16 ch write_jvm_attribute jvmal

let write_jvm_field ch jvmf =
	write_ui16 ch jvmf.field_access_flags;
	write_ui16 ch jvmf.field_name_index;
	write_ui16 ch jvmf.field_descriptor_index;
	write_jvm_attributes ch jvmf.field_attributes

let write_jvm_class ch jvmc =
	write_byte ch 0xCA;
	write_byte ch 0xFE;
	write_byte ch 0xBA;
	write_byte ch 0xBE;
	write_ui16 ch jvmc.class_minor_version;
	write_ui16 ch jvmc.class_major_version;
	write_bytes ch jvmc.class_constant_pool;
	write_ui16 ch jvmc.class_access_flags;
	write_ui16 ch jvmc.class_this_class;
	write_ui16 ch jvmc.class_super_class;
	write_ui16 ch (Array.length jvmc.class_interfaces);
	Array.iter (write_ui16 ch) jvmc.class_interfaces;
	write_ui16 ch (Array.length jvmc.class_fields);
	Array.iter (write_jvm_field ch) jvmc.class_fields;
	write_ui16 ch (Array.length jvmc.class_methods);
	Array.iter (write_jvm_field ch) jvmc.class_methods;
	write_jvm_attributes ch jvmc.class_attributes

(* Level 2: Targeting JVM structures *)

let write_exception ch jvme =
	write_ui16 ch jvme.exc_start_pc;
	write_ui16 ch jvme.exc_end_pc;
	write_ui16 ch jvme.exc_handler_pc;
	match jvme.exc_catch_type with
	| None -> write_ui16 ch 0
	| Some t -> write_ui16 ch t

let write_opcode ch code =
  let w = write_byte ch in
  let wr i32 = write_byte ch (Int32.to_int i32) in
  (* TODO: probably don't need these *)
  let bp i =
    w ((i lsr 8) land 0xFF);
    w (i land 0xFF);
  in
  let b4 i =
    w ((i lsr 24) land 0xFF);
    w ((i lsr 16) land 0xFF);
    w ((i lsr 8) land 0xFF);
    w (i land 0xFF);
  in
  let b4r i32 =
    wr (Int32.logand (Int32.shift_right_logical i32 24) i320xFF);
    wr (Int32.logand (Int32.shift_right_logical i32 16) i320xFF);
    wr (Int32.logand (Int32.shift_right_logical i32 8) i320xFF);
    wr (Int32.logand i32 i320xFF);
  in
  let rec loop code = match code with
    (* double *)
    | OpD2f -> w 0x90
    | OpD2i -> w 0x8e
    | OpD2l -> w 0x8f
    | OpDadd -> w 0x63
    | OpDaload -> w 0x31
    | OpDastore -> w 0x52
    | OpDcmpg -> w 0x98
    | OpDcmpl -> w 0x97
    | OpDdiv -> w 0x6f
    | OpDconst_0 -> w 0xe
    | OpDconst_1 -> w 0xf
    | OpDload_0 -> w 0x26
    | OpDload_1 -> w 0x27
    | OpDload_2 -> w 0x28
    | OpDload_3 -> w 0x29
    | OpDload i -> w 0x18; w i
    | OpDmul -> w 0x6b
    | OpDneg -> w 0x77
    | OpDrem -> w 0x73
    | OpDreturn -> w 0xaf
    | OpDstore_0 -> w 0x47
    | OpDstore_1 -> w 0x48
    | OpDstore_2 -> w 0x49
    | OpDstore_3 -> w 0x4a
    | OpDstore i -> w 0x39; w i
    | OpDsub -> w 0x67
    (* float *)
    | OpF2d -> w 0x8d
    | OpF2i -> w 0x8b
    | OpF2l -> w 0x8c
    | OpFadd -> w 0x62
    | OpFaload -> w 0x30
    | OpFastore -> w 0x51
    | OpFcmpg -> w 0x96
    | OpFcmpl -> w 0x95
    | OpFdiv -> w 0x6e
    | OpFconst_0 -> w 0xb
    | OpFconst_1 -> w 0xc
    | OpFconst_2 -> w 0xd
    | OpFload_0 -> w 0x22
    | OpFload_1 -> w 0x23
    | OpFload_2 -> w 0x24
    | OpFload_3 -> w 0x25
    | OpFload i -> w 0x17; w i
    | OpFmul -> w 0x6a
    | OpFneg -> w 0x76
    | OpFrem -> w 0x72
    | OpFreturn -> w 0xae
    | OpFstore_0 -> w 0x43
    | OpFstore_1 -> w 0x44
    | OpFstore_2 -> w 0x45
    | OpFstore_3 -> w 0x46
    | OpFstore i -> w 0x38; w i
    | OpFsub -> w 0x66
    (* int *)
    | OpI2b -> w 0x91
    | OpI2c -> w 0x92
    | OpI2d -> w 0x87
    | OpI2f -> w 0x86
    | OpI2l -> w 0x85
    | OpI2s -> w 0x93
    | OpIadd -> w 0x60
    | OpIaload -> w 0x2e
    | OpIand -> w 0x7e
    | OpIastore -> w 0x4f
    | OpIconst_m1 -> w 0x2
    | OpIconst_0 -> w 0x3
    | OpIconst_1 -> w 0x4
    | OpIconst_2 -> w 0x5
    | OpIconst_3 -> w 0x6
    | OpIconst_4 -> w 0x7
    | OpIconst_5 -> w 0x8
    | OpIdiv -> w 0x6c
    | OpIload_0 -> w 0x1a
    | OpIload_1 -> w 0x1b
    | OpIload_2 -> w 0x1c
    | OpIload_3 -> w 0x1d
    | OpIload i -> w 0x15; w i
    | OpImul -> w 0x68
    | OpIneg -> w 0x74
    | Op