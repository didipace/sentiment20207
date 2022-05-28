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
	writ