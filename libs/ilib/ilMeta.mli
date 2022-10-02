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
	mutable ms_id : int;
	mutable ms_semantic : semantic_flags;
	mutable ms_method : meta_method; (* Method rid *)
	mutable ms_association : has_semantics;
}

and meta_method_impl = {
	mutable mi_id : int;
	mutable mi_class : meta_type_def; (* TypeDef rid *)
	mutable mi_method_body : method_def_or_ref;
		(* overriding method *)
	mutable mi_method_declaration : method_def_or_ref;
		(* overridden method *)
}

and meta_module_ref = {
	mutable modr_id : int;
	mutable modr_name : stringref;
}

and meta_type_spec = {
	mutable ts_id : int;
	mutable ts_signature : ilsig;
}

(* reserved ? *)
and meta_enc_log = {
	mutable el_id : int;
	mutable el_token : to_det;
	mutable el_func_code : to_det;
}

and meta_impl_map = {
	mutable im_id : int;
	mutable im_flags : impl_flags; (* mapping_flags *)
	mutable im_forwarded : member_forwarded; (* method only *)
	mutable im_import_name : stringref;
	mutable im_import_scope : meta_module_ref; (* ModuleRef rid *)
}

(* reserved ? *)
and meta_enc_map = {
	mutable encm_id : int;
	mutable encm_token : to_det;
}

and meta_field_rva = {
	mutable fr_id : int;
	mutable fr_rva : rva;
	mutable fr_field : meta_field; (* Field rid *)
}

and meta_assembly = {
	mutable a_id : int;
	mutable a_hash_algo : hash_algo;
	mutable a_major : int;
	mutable a_minor : int;
	mutable a_build : int;
	mutable a_rev : int;
	mutable a_flags : assembly_flags; (* assembly_flags *)
	mutable a_public_key : blobref;
	mutable a_name : stringref;
	mutable a_locale : stringref;
}

(* unused *)
and meta_assembly_processor = {
	mutable ap_id : int;
	mutable ap_processor : to_det;
}

(* unused *)
and meta_assembly_os = {
	mutable aos_id : int;
	mutable aos_platform_id : to_det;
	mutable aos_major_version : to_det;
	mutable aos_minor_version : to_det;
}

and meta_assembly_ref = {
	mutable ar_id : int;
	mutable ar_major : int;
	mutable ar_minor : int;
	mutable ar_build : int;
	mutable ar_rev : int;
	mutable ar_flags : assembly_flags;
	mutable ar_public_key : blobref;
	mutable ar_name : stringref; (* no path, no extension *)
	mutable ar_locale : stringref;
	mutable ar_hash_value : blobref;
}

(* unused *)
and meta_assembly_ref_processor = {
	mutable arp_id : int;
	mutable arp_processor : to_det;
	mutable arp_assembly_ref : meta_assembly_ref; (* AssemblyRef rid *)
}

(* unused *)
and meta_assembly_ref_os = {
	mutable aros_id : int;
	mutable aros_platform_id : to_det;
	mutable aros_major : int;
	mutable aros_minor : int;
	mutable aros_assembly_ref : meta_assembly_ref; (* AssemblyRef rid *)
}

and meta_file = {
	mutable file_id : int;
	mutable file_flags : file_flag; (* file_flags *)
	mutable file_name : stringref; (* no path; only file name *)
	mutable file_hash_value : blobref;
}

and meta_exported_type = {
	mutable et_id : int;
	mutable et_flags : type_def_flags;
	mutable et_type_def_id : int;
		(* TypeDef token in another module *)
	mutable et_type_name : stringref;
	mutable et_type_namespace : ns;
	mutable et_implementation : implementation;
}

and meta_manifest_resource = {
	mutable mr_id : int;
	mutable mr_offset : int;
	mutable mr_flags : manifest_resource_flag; (* manifest_resource_flags *)
	mutable mr_name : stringref;
	mutable mr_implementation : implementation option;
}

and meta_nested_class = {
	mutable nc_id : int;
	mutable nc_nested : meta_type_def; (* TypeDef rid *)
	mutable nc_enclosing : meta_type_def; (* TypeDef rid *)
}

and meta_generic_param = {
	mutable gp_id : int;
	mutable gp_number : int; (* ordinal *)
	mutable gp_flags : generic_flags;
	mutable gp_owner : type_or_method_def;
		(* generic type or method *)
	mutable gp_name : stringref option;
}

and meta_method_spec = {
	mutable mspec_id : int;
	mutable mspec_method : method_def_or_ref;
		(* instantiated method *)
	mutable mspec_instantiation : ilsig;
		(* instantiated signature *)
}

and meta_generic_param_constraint = {
	mutable gc_id : int;
	mutable gc_owner : meta_generic_param; (* GenericParam rid *)
		(* constrained parameter *)
	mutable gc_constraint : type_def_or_ref;
		(* type the parameter must extend or implement *)
}

and to_det = int

and not_implemented = int

and constant =
	| IBool of bool
	| IChar of int
	| IByte of int
	| IShort of int
	| IInt of int32
	| IInt64 of int64
	| IFloat32 of float
	| IFloat64 of float
	| IString of string
	| INull

and instance =
	| InstConstant of constant
	| InstBoxed of instance
	| InstType of string
	| InstArray of instance list
	| InstEnum of int

and constant_type =
	| CBool (* 0x2 *)
	| CChar (* 0x3 *)
	| CInt8 (* 0x4 *)
	| CUInt8 (* 0x5 *)
	| CInt16 (* 0x6 *)
	| CUInt16 (* 0x7 *)
	| CInt32 (* 0x8 *)
	| CUInt32 (* 0x9 *)
	| CInt64 (* 0xA *)
	| CUInt64 (* 0xB *)
	| CFloat32 (* 0xC *)
	| CFloat64 (* 0xD *)
	| CString (* 0xE *)
	| CNullRef (* 0x12 *)
		(* null object reference - the value of the constant *)
		(* of this type must be a 4-byte integer containing 0 *)

and type_def_vis =
	(* visibility flags - mask 0x7 *)
	| VPrivate (* 0x0 *)
		(* type is not visible outside the assembly. default *)
	| VPublic (* 0x1 *)
		(* type visible outside the assembly *)
	| VNestedPublic (* 0x2 *)
		(* the nested type has public visibility *)
	| VNestedPrivate (* 0x3 *)
		(* nested type has private visibility - it's not visible outside the enclosing class *)
	| VNestedFamily (* 0x4 *)
		(* nested type has family visibility - it's visible to descendants of the enclosing class only *)
	| VNestedAssembly (* 0x5 *)
		(* nested type visible within the assembly only *)
	| VNestedFamAndAssem (* 0x6 *)
		(* nested type is visible to the descendants of the enclosing class residing in the same assembly *)
	| VNestedFamOrAssem (* 0x7 *)
		(* nested type is visible to the descendants of the enclosing class either within *)
		(* or outside the assembly and to every type within the assembly *)
	
and type_def_layout =
	(* layout flags - mask 0x18 *)
	| LAuto (* 0x0 *)
		(* type fields are laid out automatically *)
	| LSequential (* 0x8 *)
		(* loader must preserve the order of the instance fields *)
	| LExplicit (* 0x10 *)
		(* type layout is specified explicitly *)

and type_def_semantics =
	(* semantics flags - mask 0x5A0 *)
	| SInterface (* 0x20 *)
		(* type is an interface. If specified, the default parent is set to nil *)
	| SAbstract (* 0x80 *)
	| SSealed (* 0x100 *)
	| SSpecialName (* 0x400 *)
		(* type has a special name. how special depends on the name itself *)
		(* e.g. .ctor or .cctor *)

and type_def_impl =
	(* type implementation flags - mask 0x103000 *)
	| IImport (* 0x1000 *)
		(* the type is imported from a COM type library *)
	| ISerializable (* 0x2000 *)
		(* the type can be serialized into sequential data *)
	| IBeforeFieldInit (* 0x00100000 *)
		(* the type can be initialized any time before the first access *)
		(* to a static field. *)
	
and type_def_string =
	(* string formatting flags - mask 0x00030000 *)
	| SAnsi (* 0x0 *)
		(* managed strings are marshaled to and from ANSI strings *)
	| SUnicode (* 0x00010000 *)
		(* managed strings are marshaled to and from UTF-16 *)
	| SAutoChar (* 0x00020000 *)
		(* marshaling is defined by the underlying platform *)

and type_def_flags = {
	tdf_vis : type_def_vis;
	tdf_layout : type_def_layout;
	tdf_semantics : type_def_semantics list;
	tdf_impl : type_def_impl list;
	tdf_string : type_def_string;
}

and field_access =
	(* access flags - mask 0x07 *)
	| FAPrivateScope (* 0x0 *)
		(* default - exempt from the requirement of having a unique triad of owner, name and signature *)
		(* so it must always be referenced by a FieldDef token and never by a MemberRef *)
		(* privatescope fields are accessible from anywhere within the current module *)
	| FAPrivate (* 0x1 *)
		(* field is accessible from its owner and from classes nested in the field's owner. *)
		(* global private fields are accessible from anywhere within current module *)
	| FAFamAndAssem (* 0x2 *)
		(* accessible from types belonging to the owner's family defined in the current assembly *)
		(* family means the type itself and all its descendants *)
	| FAAssembly (* 0x3 *)
		(* accessible from types defined in the current assembly *)
	| FAFamily (* 0x4 *)
		(* accessible from the owner's family - defined in this or any other assembly *)
	| FAFamOrAssem (* 0x5 *)
		(* accessible from the owner's family and from all types defined in the current assembly *)
	| FAPublic (* 0x6 *)
		(* field is accessible from any type *)

and field_contract =
	(* contract flags - mask 0x02F0 *)
	| CStatic (* 0x10 *)
		(* static field. global fields must be static *)
	| CInitOnly (* 0x20 *)
		(* field can be initialized only and cannot be written to later. *)
		(* Initialization takes place in an instance constructor (.ctor) for instance fields *)
		(* and in a class constructor (.cctor) for static fields. *)
		(* this flag is not enforced by the CLR *)
	| CLiteral (* 0x40 *)
		(* field is a compile-time constant. the loader does not lay out this field *)
		(* and does not create an internal handle for it *)
		(* it cannot be directly addressed from IL and can only be used as a Reflection reference *)
	| CNotSerialized (* 0x80 *)
		(* field is not serialized when the owner is remoted *)
	| CSpecialName (* 0x200 *)
		(* the field is special in some way, as defined by its name *)
		(* example is the field value__ of an enumeration type *)

and field_reserved = 
	(* reserved flags - cannot be set explicitly. mask 0x9500 *)
	| RSpecialName (* 0x400 *)
		(* has a special name that is reserved for internal use of the CLR *)
		(* two field names are reserved: value_, for instance fields in enumerations *)
		(* and _Deleted* for fields marked for deletion but not actually removed from metadata *)
	| RMarshal (* 0x1000 *)
		(* The field has an associated FieldMarshal record specifying how the field must be *)
		(* marshaled when consumed by unmanaged code. *)
	| RConstant (* 0x8000 *)
		(* field has an associated Constant record *)
	| RFieldRVA (* 0x0100 *)
		(* field is mapped to data and has an associated FieldRVA record *)

and field_flags = {
	ff_access : field_access;
	ff_contract : field_contract list;
	ff_reserved : field_reserved list;
}

and method_contract =
	(* contract flags - mask 0xF0 *)
	| CMStatic (* 0x10 *)
	| CMFinal (* 0x20 *)
		(* must be paired with the virtual flag - otherwise it is meaningless *)
	| CMVirtual (* 0x40 *)
	| CMHideBySig (* 0x80 *)
		(* the method hides all methods of the parent classes that have a matching *)
		(* signature and name (as opposed to having a matching name only). ignored by the CLR *)

and method_vtable =
	(* vtable flags - mask 0x300 *)
	| VNewSlot (* 0x100 *)
		(* a new vtable slot is created, so it doesn't override the old implementation *)
	| VStrict (* 0x200 *)
		(* virtual method can be overridden only if it is accessible from the overriding class *)

and method_impl =
	(* implementation flags - mask 0x2C08 *)
	| IAbstract (* 0x0400 *)
	| ISpecialName (* 0x0800 *)
	| IPInvokeImpl (* 0x2000 *)
		(* the method has an unmanaged implementation and is called through the platform *)
		(* invocation mechanism. the rva field must be 0, since the method is implemented externally *)
	| IUnmanagedExp (* 0x0008 *)
		(* the managed method is exposed as an unmanaged export. not used by the CLR currently *)

and method_reserved =
	(* reserved flags - cannot be set explicitly. mask 0xD000 *)
	| RTSpecialName (* 0x1000 *)
		(* has a special name: .ctor, .cctor, _VtblGap* and _Deleted* *)
	| RHasSecurity (* 0x4000 *)
		(* either has an associated DeclSecurity metadata or the custom attribte *)
		(* System.Security.SuppressUnmanagedCodeSecurityAttribute *)
	| RReqSecObj (* 0x8000 *)
		(* this method calls another method containing security code, so it requires *)
		(* an additional stack slot for a security object. *)

and method_code_type =
	(* code type - mask 0x3 *)
	| CCil (* 0x0 *)
	| CNative (* 0x1 *)
		(* implemented in native platform-specific code *)
	| COptIl (* 0x2 *)
		(* optimized il - not supported; must not be set *)
	| CRuntime (* 0x3 *)
		(* automatically generated by the runtime itself (intrinsic) *)

and method_code_mngmt =
	(* code management - mask 0x4 *)
	| MManaged (* 0x0 *)
	| MUnmanaged (* 0x4 *)
		(* must be paired with the native flag *)

and method_interop =
	(* method implementation and interop - mask 0x10D8 *)
	| OForwardRef (* 0x10 *)
		(* managed object fiels and edit-and-continue scenarios only *)
	| OPreserveSig (* 0x80 *)
		(* method signa