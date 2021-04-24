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

(* Various checks performed while loading types. *)

open Globals
open Ast
open Type
open Typecore
open DisplayException
open DisplayTypes
open DisplayMode
open CompletionItem
open CompletionModuleKind
open CompletionModuleType
open CompletionResultKind
open Common
open Error

exception Build_canceled of build_state

let is_generic_parameter ctx c =
	(* first check field parameters, then class parameters *)
	let name = snd c.cl_path in
	try
		ignore(lookup_param name ctx.curfield.cf_params);
		has_class_field_flag ctx.curfield CfGeneric
	with Not_found -> try
		ignore(lookup_param name ctx.type_params);
		(match ctx.curclass.cl_kind with | KGeneric -> true | _ -> false);
	with Not_found ->
		false

let valid_redefinition ctx map1 map2 f1 t1 f2 t2 = (* child, parent *)
	let valid t1 t2 =
		Type.unify t1 t2;
		if is_null t1 <> is_null t2 || ((follow t1) == t_dynamic && (follow t2) != t_dynamic) then raise (Unify_error [Cannot_unify (t1,t2)]);
	in
	let open OptimizerTexpr in
	begin match PurityState.get_purity_from_meta f2.cf_meta,PurityState.get_purity_from_meta f1.cf_meta with
		| PurityState.Pure,PurityState.MaybePure -> f1.cf_meta <- (Meta.Pure,[EConst(Ident "expect"),f2.cf_pos],null_pos) :: f1.cf_meta
		| PurityState.ExpectPure p,PurityState.MaybePure -> f1.cf_meta <- (Meta.Pure,[EConst(Ident "expect"),p],null_pos) :: f1.cf_meta
		| _ -> ()
	end;
	let t1, t2 = (match f1.cf_params, f2.cf_params with
		| [], [] -> t1, t2
		| l1, l2 when List.length l1 = List.length l2 ->
			let to_check = ref [] in
			(* TPTODO: defaults *)
			let monos = List.map2 (fun tp1 tp2 ->
				(match follow tp1.ttp_type, follow tp2.ttp_type with
				| TInst ({ cl_kind = KTypeParameter ct1 } as c1,pl1), TInst ({ cl_kind = KTypeParameter ct2 } as c2,pl2) ->
					(match ct1, ct2 with
					| [], [] -> ()
					| _, _ when List.length ct1 = List.length ct2 ->
						(* if same constraints, they are the same type *)
						let check monos =
							List.iter2 (fun t1 t2  ->
								try
									let t1 = apply_params l1 monos (apply_params c1.cl_params pl1 (map2 t1)) in
									let t2 = apply_params l2 monos (apply_params c2.cl_params pl2 (map1 t2)) in
									type_eq EqStrict t1 t2
								with Unify_error l ->
									raise (Unify_error (Unify_custom "Constraints differ" :: l))
							) ct1 ct2
						in
						to_check := check :: !to_check;
					| _ ->
						raise (Unify_error [Unify_custom "Different number of constraints"]))
				| _ -> ());
				TInst (mk_class null_module ([],tp1.ttp_name) null_pos null_pos,[])
			) l1 l2 in
			List.iter (fun f -> f monos) !to_check;
			apply_params l1 monos t1, apply_params l2 monos t2
		| _  ->
			(* ignore type params, will create other errors later *)
			t1, t2
	) in
	match f1.cf_kind,f2.cf_kind with
	| Method m1, Method m2 when not (m1 = MethDynamic) && not (m2 = MethDynamic) ->
		begin match follow t1, follow t2 with
		| TFun (args1,r1) , TFun (args2,r2) -> (
			if not (List.length args1 = List.length args2) then raise (Unify_error [Unify_custom "Different number of function arguments"]);
			let i = ref 0 in
			try
				valid r1 r2;
				List.iter2 (fun (n,o1,a1) (_,o2,a2) ->
					incr i;
					if o1 <> o2 then raise (Unify_error [Not_matching_optional n]);
					(try valid a2 a1 with Unify_error _ -> raise (Unify_error [Cannot_unify(a1,a2)]))
				) args1 args2;
			with Unify_error l ->
				let msg = if !i = 0 then Invalid_return_type else Invalid_function_argument(!i,List.length args1) in
				raise (Unify_error (Cannot_unify (t1,t2) :: msg :: l)))
		| _ ->
			die "" __LOC__
		end
	| _,(Var { v_write = AccNo | AccNever }) ->
		(* write variance *)
		valid t1 t2
	| _,(Var { v_read = AccNo | AccNever }) ->
		(* read variance *)
		valid t2 t1
	| _,_ when has_class_field_flag f2 CfFinal ->
		(* write variance *)
		valid t1 t2
	| _ , _ ->
		(* in case args differs, or if an interface var *)
		type_eq EqStrict t1 t2;
		if is_null t1 <> is_null t2 then raise (Unify_error [Cannot_unify (t1,t2)])

let copy_meta meta_src meta_target sl =
	let meta = ref meta_target in
	List.iter (fun (m,e,p) ->
		if List.mem m sl then meta := (m,e,p) :: !meta
	) meta_src;
	!meta

(** retrieve string from @:native metadata or raise Not_found *)
let get_native_name meta =
	let rec get_native meta = match meta with
		| [] -> raise Not_found
		| (Meta.Native,[v],p as meta) :: _ ->
			meta
		| _ :: meta ->
			get_native meta
	in
	let (_,e,mp) = get_native meta in
	match e with
	| [Ast.EConst (Ast.String(name,_)),p] ->
		name,p
	| [] ->
		raise Not_found
	| _ ->
		typing_error "String expected" mp

let check_native_name_override ctx child base =
	let error base_pos child_pos =
		display_error ctx.com ("Field " ^ child.cf_name ^ " has different @:native value than in superclass") child_pos;
		display_error ctx.com (compl_msg "Base field is defined here") base_pos
	in
	try
		let child_name, child_pos = get_native_name child.cf_meta in
		try
			let base_name, base_pos = get_native_name base.cf_meta in
			if base_name <> child_name then
				error base_pos child_pos
		with Not_found ->
			error base.cf_name_pos child_pos
	with Not_found -> ()

let check_overriding ctx c f =
	match c.cl_super with
	| None ->
		if has_class_field_flag f CfOverride then display_error ctx.com ("Field " ^ f.cf_name ^ " is declared 'override' but doesn't override any field") f.cf_pos
	| _ when (has_class_flag c CExtern) && Meta.has Meta.CsNative c.cl_meta -> () (* -net-lib specific: do not check overrides on extern CsNative classes *)
	| Some (csup,params) ->
		let p = f.cf_name_pos in
		let i = f.cf_name in
		let check_field f get_super_field is_overload = try
			(if is_overload && not (has_class_field_flag f CfOverload) then
				display_error ctx.com ("Missing overload declaration for field " ^ i) p);
			let f_has_override = has_class_field_flag f CfOverride in
			let t, f2 = get_super_field csup i in
			check_native_name_override ctx f f2;
			(* allow to define fields that are not defined for this platform version in superclass *)
			(match f2.cf_kind with
			| Var { v_read = AccRequire _ } -> raise Not_found;
			| _ -> ());
			if has_class_field_flag f2 CfAbstract then begin
				if f_has_override then
					display_error ctx.com ("Field " ^ i ^ " is declared 'override' but parent field " ^ i ^ " is 'abstract' and does not provide any implementation to override") p
				else
					add_class_field_flag f CfOverride (* our spec requires users to not "override" abstract functions, but our implementation depends on implementations to be declared with "override" ¯\_(ツ)_/¯ *)
			end;
			if (has_class_field_flag f2 CfOverload && not (has_class_field_flag f CfOverload)) then
				display_error ctx.com ("Field " ^ i ^ " should be declared with overload since it was already declared as overload in superclass") p
			else if not f_has_override && not (has_class_field_flag f2 CfAbstract) then begin
				if has_class_flag c CExtern then add_class_field_flag f CfOverride
				else display_error ctx.com ("Field " ^ i ^ " should be declared with 'override' since it is inherited from superclass " ^ s_type_path csup.cl_path) p
			end else if not (has_class_field_flag f CfPublic) && (has_class_field_flag f2 CfPublic) then
				display_error ctx.com ("Field " ^ i ^ " has less visibility (public/private) than superclass one") p
			else (match f.cf_kind, f2.cf_kind with
			| _, Method MethInline ->
				display_error ctx.com ("Field " ^ i ^ " is inlined and cannot be overridden") p
			| a, b when a = b -> ()
			| Method MethInline, Method MethNormal ->
				() (* allow to redefine a method as inlined *)
			| _ ->
				display_error ctx.com ("Field " ^ i ^ " has different property access than in superclass") p);
			if (has_class_field_flag f2 CfFinal) then display_error ctx.com ("Cannot override final method " ^ i) p;
			try
				let t = apply_params csup.cl_params params t in
				let map = TClass.get_map_function csup params in
				valid_redefinition ctx map map f f.cf_type f2 t;
			with
				Unify_error l ->
					display_error ctx.com ("Field " ^ i ^ " overrides parent class with different or incomplete type") p;
					display_error ctx.com (compl_msg "Base field is defined here") f2.cf_name_pos;
					display_error ctx.com (compl_msg (error_msg (Unify l))) p;
		with
			Not_found ->
				if has_class_field_flag f CfOverride then
					let msg = if is_overload then
						("Field " ^ i ^ " is declared 'override' but no compatible overload was found")
					else begin
						let fields = TClass.get_all_super_fields c in
						let fields = PMap.fold (fun (_,cf) acc -> match cf.cf_kind with
							| Method MethNormal when not (has_class_field_flag cf CfFinal) -> cf.cf_name :: acc
							| _ -> acc
						) fields [] in
						StringError.string_error i fields ("Field " ^ i ^ " is declared 'override' but doesn't override any field")
					end in
					display_error ctx.com msg p
		in
		if has_class_field_flag f CfOverload then begin
			let overloads = Ov