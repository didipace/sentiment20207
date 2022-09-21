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

(* starting with all annotations of special coded t