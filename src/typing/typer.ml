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
open Ast
open DisplayTypes.DisplayMode
open DisplayException
open DisplayTypes.CompletionResultKind
open CompletionItem.ClassFieldOrigin
open Common
open Type
open Typecore
open Error
open Globals
open TyperBase
open Fields
open CallUnification
open Calls
open Operators

(* ---------------------------------------------------------------------- *)
(* TOOLS *)

let mono_or_dynamic ctx with_type p = match with_type with
	| WithType.NoValue ->
		t_dynamic
	| Value _ | WithType _ ->
		spawn_monomorph ctx p

let get_iterator_param t =
	match follow t with
	| TAnon a ->
		if !(a.a_status) <> Closed then raise Not_found;
		(match follow (PMap.find "hasNext" a.a_fields).cf_type, follow (PMap.find "next" a.a_fields).cf_type with
		| TFun ([],tb), TFun([],t) when (match follow tb with TAbstract ({ a_path = [],"Bool" },[]) -> true | _ -> false) ->
			if PMap.fold (fun _ acc -> acc + 1) a.a_fields 0 <> 2 then raise Not_found;
			t
		| _ ->
			raise Not_found)
	| _ ->
		raise Not_found

let get_iterable_param t =
	match follow t with
	| TAnon a ->
		if !(a.a_status) <> Closed then raise Not_found;
		(match follow (PMap.find "iterator" a.a_fields).cf_type with
		| TFun ([],it) ->
			let t = get_iterator_param it in
			if PMap.fold (fun _ acc -> acc + 1) a.a_fields 0 <> 1 then raise Not_found;
			t
		| _ ->
			raise Not_found)
	| _ -> raise Not_found

let maybe_type_against_enum ctx f with_type iscall p =
	try
		begin match with_type with
		| WithType.WithType(t,_) ->
			let rec loop stack t = match follow t with
				| TEnum (en,_) ->
					true,en.e_path,en.e_names,TEnumDecl en
				| TAbstract ({a_impl = Some c} as a,_) when a.a_enum ->
					let fields = ExtList.List.filter_map (fun cf ->
						if has_class_field_flag cf CfEnum then Some cf.cf_name else None
					) c.cl_ordered_statics in
					false,a.a_path,fields,TAbstractDecl a
				| TAbstract (a,pl) when not (Meta.has Meta.CoreType a.a_meta) ->
					begin match get_abstract_froms ctx a pl with
						| [t2] ->
							if (List.exists (shallow_eq t) stack) then raise Exit;
							loop (t :: stack) t2
						| _ -> raise Exit
					end
				| _ ->
					raise Exit
			in
			let is_enum,path,fields,mt = loop [] t in
			let old = ctx.m.curmod.m_types in
			let restore () = ctx.m.curmod.m_types <- old in
			ctx.m.curmod.m_types <- ctx.m.curmod.m_types @ [mt];
			let e = try
				f()
			with
			| Error (Unknown_ident n,_) ->
				restore();
				raise_or_display_message ctx (StringError.string_error n fields ("Identifier '" ^ n ^ "' is not part of " ^ s_type_path path)) p;
				AKExpr (mk (TConst TNull) (mk_mono()) p)
			| exc ->
				restore();
				raise exc;
			in
			restore();
			begin match e with
				| AKExpr e ->
					begin match follow e.etype with
						| TFun(_,t') when is_enum ->
							(* TODO: this is a dodge for #7603 *)
							(try Type.unify t' t with Unify_error _ -> ());
							AKExpr e
						| _ ->
							AKExpr e
					end
				| _ -> e (* ??? *)
			end
		| _ ->
			raise Exit
		end
	with Exit ->
		f()

let check_error ctx err p = match err with
	| Module_not_found ([],name) when Diagnostics.error_in_diagnostics_run ctx.com p ->
		DisplayToplevel.handle_unresolved_identifier ctx name p true
	| _ ->
		display_error ctx.com (error_ms