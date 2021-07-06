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
		display_error ctx.com (error_msg err) p

(* ---------------------------------------------------------------------- *)
(* PASS 3 : type expression & check structure *)

let rec unify_min_raise ctx (el:texpr list) : t =
	let basic = ctx.com.basic in
	match el with
	| [] -> spawn_monomorph ctx null_pos
	| [e] -> e.etype
	| _ ->
		let rec chk_null e = is_null e.etype || is_explicit_null e.etype ||
			match e.eexpr with
			| TConst TNull -> true
			| TBlock el ->
				(match List.rev el with
				| [] -> false
				| e :: _ -> chk_null e)
			| TParenthesis e | TMeta(_,e) -> chk_null e
			| _ -> false
		in
		(* First pass: Try normal unification and find out if null is involved. *)
		let rec loop t = function
			| [] ->
				false, t
			| e :: el ->
				let t = if chk_null e then basic.tnull t else t in
				try
					Type.unify e.etype t;
					loop t el
				with Unify_error _ -> try
					Type.unify t e.etype;
					loop (if is_null t then basic.tnull e.etype else e.etype) el
				with Unify_error _ ->
					true, t
		in
		let has_error, t = loop (spawn_monomorph ctx null_pos) el in
		if not has_error then
			t
		else try
			(* specific case for const anon : we don't want to hide fields but restrict their common type *)
			let fcount = ref (-1) in
			let field_count a =
				PMap.fold (fun _ acc -> acc + 1) a.a_fields 0
			in
			let expr f = match f.cf_expr with None -> mk (TBlock []) f.cf_type f.cf_pos | Some e -> e in
			let fields = List.fold_left (fun acc e ->
				match follow e.etype with
				| TAnon a when !(a.a_status) = Const ->
					if !fcount = -1 then begin
						fcount := field_count a;
						PMap.map (fun f -> [expr f]) a.a_fields
					end else begin
						if !fcount <> field_count a then raise Not_found;
						PMap.mapi (fun n el -> expr (PMap.find n a.a_fields) :: el) acc
					end
				| _ ->
					raise Not_found
			) PMap.empty el in
			let fields = PMap.foldi (fun n el acc ->
				let t = try unify_min_raise ctx el with Unify_error _ -> raise Not_found in
				PMap.add n (mk_field n t (List.hd el).epos null_pos) acc
			) fields PMap.empty in
			mk_anon ~fields (ref Closed)
		with Not_found -> try
			(* specific case for TFun, see #9579 *)
			let e0,el = match el with
				| e0 :: el -> e0,el
				| _ -> raise Exit
			in
			let args,tr0 = match follow e0.etype with
				| TFun(tl,tr) ->
					Array.of_list tl,tr
				| _ ->
					raise Exit
			in
			let arity = Array.length args in
			let rets = List.map (fun e -> match follow e.etype with
				| TFun(tl,tr) ->
					let ta = Array.of_list tl in
					if Array.length ta <> arity then raise Exit;
					for i = 0 to arity - 1 do
						let (_,_,tcur) = args.(i) in
						let (_,_,tnew) as argnew = ta.(i) in
						if Type.does_unify tnew tcur then
							args.(i) <- argnew
						else if not (Type.does_unify tcur tnew) then
							raise Exit
					done;
					tr
				| _ ->
					raise Exit
			) el in
			let common_types = UnifyMinT.collect_base_types tr0 in
			let tr = match UnifyMinT.unify_min' default_unification_context common_types rets with
			| UnifyMinOk t ->
				t
			| UnifyMinError(l,index) ->
				raise Exit
			in
			TFun(Array.to_list args,tr)
		with Exit ->
			(* Second pass: Get all base types (interfaces, super classes and their interfaces) of most general type.
			   Then for each additional type filter all types that do not unify. *)
			let common_types = UnifyMinT.collect_base_types t in
			let dyn_types = List.fold_left (fun acc t ->
				let rec loop c =
					Meta.has Meta.UnifyMinDynamic c.cl_meta || (match c.cl_super with None -> false | Some (c,_) -> loop c)
				in
				match t with
				| TInst (c,params) when params <> [] && loop c ->
					TInst (c,List.map (fun _ -> t_dynamic) params) :: acc
				| _ -> acc
			) [] common_types in
			let common_types = (match List.rev dyn_types with [] -> common_types | l -> common_types @ l) in
			let el = List.tl el in
			let tl = List.map (fun e -> e.etype) el in
			begin match UnifyMinT.unify_min' default_unification_context common_types tl with
			| UnifyMinOk t ->
				t
			| UnifyMinError(l,index) ->
				raise_typing_error (Unify l) (List.nth el index).epos
			end

let unify_min ctx el =
	try unify_min_raise ctx el
	with Error (Unify l,p) ->
		if not ctx.untyped then display_error ctx.com (error_msg (Unify l)) p;
		(List.hd el).etype

let unify_min_for_type_source ctx el src =
	match src with
	| Some WithType.ImplicitReturn when List.exists (fun e -> ExtType.is_void (follow e.etype)) el ->
		ctx.com.basic.tvoid
	| _ ->
		unify_min ctx el

let rec type_ident_raise ctx i p mode with_type =
	let is_set = match mode with MSet _ -> true | _ -> false in
	match i with
	| "true" ->
		let acc = AKExpr (mk (TConst (TBool true)) ctx.t.tbool p) in
		if mode = MGet then
			acc
		else
			AKNo(acc,p)
	| "false" ->
		let acc = AKExpr (mk (TConst (TBool false)) ctx.t.tbool p) in
		if mode = MGet then
			acc
		else
			AKNo(acc,p)
	| "this" ->
		let acc = AKExpr(get_this ctx p) in
		begin match mode with
		| MSet _ ->
			add_class_field_flag ctx.curfield CfModifiesThis;
			begin match ctx.curclass.cl_kind with
			| KAbstractImpl _ ->
				if not (assign_to_this_is_allowed ctx) then
					typing_error "Abstract 'this' value can only be modified inside an inline function" p;
				acc
			| _ ->
				AKNo(acc,p)
			end
		| MCall _ ->
			begin match ctx.curclass.cl_kind with
			| KAbstractImpl _ ->
				acc
			| _ ->
				AKNo(acc,p)
			end
		| MGet ->
			acc
		end;
	| "abstract" ->
		begin match mode, ctx.curclass.cl_kind with
			| MSet _, KAbstractImpl ab -> typing_error "Property 'abstract' is read-only" p;
			| (MGet, KAbstractImpl ab)
			| (MCall _, KAbstractImpl ab) ->
				let tl = extract_param_types ab.a_params in
				let e = get_this ctx p in
				let e = {e with etype = TAbstract (ab,tl)} in
				AKExpr e
			| _ ->
				typing_error "Property 'abstract' is reserved and only available in abstracts" p
		end
	| "super" ->
		let t = (match ctx.curclass.cl_super with
			| None -> typing_error "Current class does not have a superclass" p
			| Some (c,params) -> TInst(c,params)
		) in
		(match ctx.curfun with
		| FunMember | FunConstructor -> ()
		| FunMemberAbstract -> typing_error "Cannot access super inside an abstract function"