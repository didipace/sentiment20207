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
	| MethodImpl of meta_method_impl
		(* method implementation descriptors *)
	| ModuleRef of meta_module_ref
		(* module reference descriptors *)
	| TypeSpec of meta_type_spec
		(* Type specification descriptors *)
	| ImplMap of meta_impl_map
		(* implementation map descriptors used for platform invocation (P/Invoke) *)
	| FieldRVA of meta_field_rva
		(* field-to-data mapping descriptors *)
	| ENCLog of meta_enc_log
		(* edit-and-continue log descriptors that hold information about what changes *)
		(* have been made to specific metadata items during in-memory editing *)
		(* this table does not exist on optimized metadata *)
	| ENCMap of meta_enc_map
		(* edit-and-continue mapping descriptors. does not exist on optimized metadata *)
	| Assembly of meta_assembly
		(* the current assembly descriptor, which should appear only in the prime module metadata *)
	| AssemblyProcessor of meta_assembly_processor | AssemblyOS of meta_assembly_os
		(* unused *)
	| AssemblyRef of meta_assembly_ref
		(* assembly reference descriptors *)
	| AssemblyRefProcessor of meta_assembly_ref_processor | AssemblyRefOS of meta_assembly_ref_os
		(* unused *)
	| File of meta_file
		(* file descriptors that contain information about other files in the current assembly *)
	| ExportedType of meta_exported_type
		(* exported type descriptors that contain information about public classes *)
		(* exported by the current assembly, which are declared in other modules of the assembly *)
		(* only the prime module of the assembly should carry this table *)
	| ManifestResource of meta_manifest_resource
		(* managed resource descriptors *)
	| NestedClass of meta_nested_class
		(* nested class descriptors that provide mapping of nested classes to their respective enclosing classes *)
	| GenericParam of meta_generic_param
		(* type parameter descriptors for generic classes and methods *)
	| MethodSpec of meta_method_spec
		(* generic method instantiation descriptors *)
	| GenericParamConstraint of meta_generic_param_constraint
		(* descriptors of constraints specified for type parameters of generic classes and methods *)
	| UnknownMeta of int

(* all fields here need to be mutable, as they will first be initialized empty *)

and meta_root = {
	root_id : int;
}

and meta_root_ptr = {
	ptr_id : int;
	ptr_to : meta_root;
}

and meta_module = {
	mutable md_id : int;
	mutable md_generation : int;
	mutable md_name : id;
	mutable md_vid : guid;
	mutable md_encid : guid;
	mutable md_encbase_id : guid;
}

and meta_type_ref = {
	mutable tr_id : int;
	mutable tr_resolution_scope : resolution_scope;
	mutable tr_name : id;
	mutable tr_namespace : ns;
}

and meta_type_def = {
	mutable td_id : int;
	mutable td_flags : type_def_flags;
	mutable td_name : id;
	mutable td_namespace : ns;
	mutable td_extends : type_def_or_ref option;
	mutable td_field_list : meta_field list;
	mutable td_method_list : meta_method list;

	(* extra field *)
	mutable td_extra_enclosing : meta_type_def option;
}

and meta_field_ptr = {
	mutable fp_id : int;
	mutable fp_field : meta_field;
}

and meta_field = {
	mutable f_id : int;
	mutable f_flags : field_flags;
	mutable f_name : id;
	mutable f_signature : ilsig;
}

and meta_method_ptr = {
	mutable mp_id : int;
	mutable mp_method : meta_method;
}

and meta_method = {
	mutable m_id : int;
	mutable m_rva : rva;
	mutable m_flags : method_flags;
	mutable m_name : id;
	mutable m_signature : ilsig;
	mutable m_param_list : meta_param list; (* rid: Param *)

	(* extra field *)
	mutable m_declaring : meta_type_def option;
}

and meta_param_ptr = {
	mutable pp_id : int;
	mutable pp_param : meta_param;
}

and meta_param = {
	mutable p_id : int;
	mutable p_flags : param_flags;
	mutable p_sequence : int;
		(* 0 means return value *)
	mutable p_name : id;
}

and meta_interface_impl = {
	mutable ii_id : int;
	mutable ii_class : meta_type_def; (* TypeDef rid *)
	mutable ii_interface : type_def_or_ref;
}

and meta_member_ref = {
	mutable memr_id : int;
	mutable memr_class : member_ref_parent;
	mutable memr_name : id;
	mutable memr_signature : ilsig;
}

and meta_constant = {
	mutable c_id : int;
	mutable c_type : constant_type;
	mutable c_parent : has_const;
	mutable c_value : constant;
}

and named_attribute = bool * string * instance (* is_property * name * instance *)

and meta_custom_attribute = {
	mutable ca_id : int;
	mutable ca_parent : has_custom_attribute;
	mutable ca_type : custom_attribute_type;
	mutable ca_value : (instance list * named_attribute list) option;
		(* can be 0 *)
}

and meta_field_marshal = {
	mutable fm_id : int;
	mutable fm_parent : has_field_marshal;
	mutable fm_native_type : nativesig;
}

and meta_decl_security = {
	mutable ds_id : int;
	mutable ds_action : action_security;
	mutable ds_parent : has_decl_security;
	mutable ds_permission_set : blobref;
		(* an xml with the permission set *)
}

and meta_class_layout = {
	mutable cl_id : int;
	mutable cl_packing_size : int;
		(* power of two; from 1 through 128 *)
	mutable cl_class_size : int;
	mutable cl_parent : meta_type_def; (* TypeDef rid *)
}

and meta_field_layout = {
	mutable fl_id : int;
	mutable fl_offset : int;
		(* offset in bytes or ordinal *)
	mutable fl_field : meta_field; (* Field rid *)
}

and meta_stand_alone_sig = {
	mutable sa_id : int;
	mutable sa_signature : ilsig;
}

and meta_event_map = {
	mutable em_id : int;
	mutable em_parent : meta_type_def; (* TypeDef rid *)
	mutable em_event_list : meta_event list; (* Event rid *)
}

and meta_event_ptr = {
	mutable ep_id : int;
	mutable ep_event : meta_event; (* Event rid *)
}

and meta_event = {
	mutable e_id : int;
	mutable e_flags : event_flags;
	mutable e_name : stringref;
	mutable e_event_type : type_def_or_ref;
}

and meta_property_map = {
	mutable pm_id : int;
	mutable pm_parent : meta_type_def; (* TypeDef rid *)
	mutable pm_property_list : meta_property list; (* Property rid *)
}

and meta_property_ptr = {
	mutable prp_id : int;
	mutable prp_property : meta_property; (* Property rid *)
}

and meta_property = {
	mutable prop_id : int;
	mutable prop_flags : property_flags;
	mutable prop_name : stringref;
	mutable prop_type : ilsig;
}

and meta_method_semantics = {
	mutable ms_