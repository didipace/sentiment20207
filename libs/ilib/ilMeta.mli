(*
 *  This file is part of ilLib
 *  Copyright (c)2004-2013 Haxe Foundation
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

open PeData;;

(* useful types for describing CLI metadata *)
type guid = string
	(* reference from the #GUID stream *)
type stringref = string
	(* reference from the #Strings stream *)
type blobref = string
	(* reference from the #Blob stream *)
type id = stringref
	(* a stringref that references an identifier. *)
	(* must begin with an alphabetic character, or the following characters: *)
		(* #, $, @, _ *)
	(* and continue with alphanumeric characters or one of the following: *)
		(* ?, $, @, _, ` *)

type ns = id list

type rid = int
	(* record id on a specified meta table *)

type clr_meta_idx =
	(* strongly-type each table index *)
	| IModule | ITypeRef | ITypeDef | IFieldPtr
	| IField | IMethodPtr | IMethod | IParamPtr
	| IParam | IInterfaceImpl | IMemberRef | IConstant
	| ICustomAttribute | IFieldMarshal | IDeclSecurity
	| IClassLayout | IFieldLayout | IStandAloneSig
	| IEventMap | IEventPtr | IEvent | IPropertyMap
	| IPropertyPtr | IProperty | IMethodSemantics
	| IMethodImpl | IModuleRef | ITypeSpec | IImplMap
	| IFieldRVA | IENCLog | IENCMap | IAssembly
	| IAssemblyProcessor | IAssemblyOS | IAssemblyRef
	| IAssemblyRefProcessor | IAssemblyRefOS
	| IFile | IExportedType | IManifestResource | INestedClass
	| IGenericParam | IMethodSpec | IGenericParamConstraint
	(* reserved metas *)
	| IR0x2D | IR0x2E | IR0x2F
	| IR0x30 | IR0x31 | IR0x32 | IR0x33 | IR0x34 | IR0x35 | IR0x36 | IR0x37 
	| IR0x38 | IR0x39 | IR0x3A | IR0x3B | IR0x3C | IR0x3D | IR0x3E | IR0x3F
	(* coded tokens *)
	| ITypeDefOrRef | IHasConstant | IHasCustomAttribute
	| IHasFieldMarshal | IHasDeclSecurity | IMemberRefParent
	| IHasSemantics | IMethodDefOrRef | IMemberForwarded | IImplementation
	| ICustomAttributeType | IResolutionScope | ITypeOrMethodDef

type meta_pointer = clr_meta_idx * rid
	(* generic reference to the meta table *)

(* starting with all annotations of special coded types *)
type type_def_or_ref = clr_meta
and has_const = clr_meta
and has_custom_attribute = clr_meta
and has_field_marshal = clr_meta
and has_decl_security = clr_meta
and member_ref_parent = clr_meta
and has_semantics = clr_meta
and method_def_or_ref = clr_meta
and member_forwarded = clr_meta
and implementation = clr_meta
and custom_attribute_type = clr_meta
and resolution_scope = clr_meta
and type_or_method_def = clr_meta

and clr_meta =
	| Module of meta_module
		(* the current module descriptor *)
	| TypeRef of meta_type_ref
		(* class reference descriptors *)
	| TypeDef of meta_type_def
		(* class or interface definition descriptors *)
	| FieldPtr of meta_field_ptr
		(* a class-to-fields lookup table - does not exist in optimized metadatas *)
	| Field of meta_field
		(* field definition descriptors *)
	| MethodPtr of meta_method_ptr
		(* a class-to-methods lookup table - does not exist in optimized metadatas *)
	| Method of meta_method
		(* method definition descriptors *)
	| ParamPtr of meta_param_ptr
		(* a method-to-parameters lookup table - does not exist in optimized metadatas *)
	| Param of meta_param
		(* parameter definition descriptors *)
	| InterfaceImpl of meta_interface_impl
		(* interface implementation descriptors *)
	| MemberRef of meta_member_ref
		(* member (field or method) reference descriptors *)
	| Constant of meta_constant
		(* constant value that map the default values stored in the #Blob stream to *)
		(* respective fields, parameters and properties *)
	| CustomAttribute of meta_custom_attribute
		(* custom attribute descriptors *)
	| FieldMarshal of meta_field_marshal
		(* field or parameter marshaling descriptors for managed/unmanaged interop *)
	| DeclSecurity of meta_decl_security
		(* security descriptors *)
	| ClassLayout of meta_class_layout	
		(* class layout descriptors that hold information about how the loader should lay out respective classes *)
	| FieldLayout of meta_field_layout
		(* field layout descriptors that specify the offset or oridnal of individual fields *)
	| StandAloneSig of meta_stand_alone_sig
		(* stand-alone signature descriptors. used in two capacities: *)
		(* as composite signatures of local variables of methods *)
		(* and as parameters of the call indirect (calli) IL instruction *)
	| EventMap of meta_event_map
		(* a class-to-events mapping table. exists also in optimized metadatas *)
	| EventPtr of meta_event_ptr
		(* an event map-to-events lookup table - does not exist in optimized metadata *)
	| Event of meta_event
		(* event descriptors *)
	| PropertyMap of meta_property_map
		(* a class-to-properties mapping table. exists also in optimized metadatas *)
	| PropertyPtr of meta_property_ptr
		(* a property map-to-properties lookup table - does not exist in optimized metadata *)
	| Property of meta_property
		(* property descriptors *)
	| MethodSemantics of meta_method_semantics
		(* method semantics descriptors that hold information about which method is associated *)
		(* with a specific property or event and in what capacity *)
	| MethodImpl of meta_meth