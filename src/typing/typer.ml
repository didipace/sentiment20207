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
		| FunMemberAbstract -> typing_error "Cannot access super inside an abstract function" p
		| FunStatic -> typing_error "Cannot access super inside a static function" p;
		| FunMemberClassLocal | FunMemberAbstractLocal -> typing_error "Cannot access super inside a local function" p);
		AKExpr (mk (TConst TSuper) t p)
	| "null" ->
		let acc =
			(* Hack for #10787 *)
			if ctx.com.platform = Cs then
				AKExpr (null (spawn_monomorph ctx p) p)
			else begin
				let tnull () = ctx.t.tnull (spawn_monomorph ctx p) in
				let t = match with_type with
					| WithType.WithType(t,_) ->
						begin match follow t with
						| TMono r ->
							(* If our expected type is a monomorph, bind it to Null<?>. *)
							Monomorph.do_bind r (tnull())
						| _ ->
							(* Otherwise there's no need to create a monomorph, we can just type the null literal
							the way we expect it. *)
							()
						end;
						t
					| _ ->
						tnull()
				in
				AKExpr (null t p)
			end
		in
		if mode = MGet then acc else AKNo(acc,p)
	| _ ->
	try
		let v = PMap.find i ctx.locals in
		(match v.v_extra with
		| Some ve ->
			let (params,e) = (ve.v_params,ve.v_expr) in
			let t = apply_params params (Monomorph.spawn_constrained_monos (fun t -> t) params) v.v_type in
			(match e with
			| Some ({ eexpr = TFunction f } as e) when ctx.com.display.dms_inline ->
				begin match mode with
					| MSet _ -> typing_error "Cannot set inline closure" p
					| MGet -> typing_error "Cannot create closure on inline closure" p
					| MCall _ ->
						(* create a fake class with a fake field to emulate inlining *)
						let c = mk_class ctx.m.curmod (["local"],v.v_name) e.epos null_pos in
						let cf = { (mk_field v.v_name v.v_type e.epos null_pos) with cf_params = params; cf_expr = Some e; cf_kind = Method MethInline } in
						add_class_flag c CExtern;
						c.cl_fields <- PMap.add cf.cf_name cf PMap.empty;
						let e = mk (TConst TNull) (TInst (c,[])) p in
						AKField (FieldAccess.create e cf (FHInstance(c,[])) true p)
				end
			| _ ->
				AKExpr (mk (TLocal v) t p))
		| _ ->
			AKExpr (mk (TLocal v) v.v_type p))
	with Not_found -> try
		(* member variable lookup *)
		if ctx.curfun = FunStatic then raise Not_found;
		let c , t , f = class_field ctx ctx.curclass (extract_param_types ctx.curclass.cl_params) i p in
		field_access ctx mode f (match c with None -> FHAnon | Some (c,tl) -> FHInstance (c,tl)) (get_this ctx p) p
	with Not_found -> try
		(* static variable lookup *)
		let f = PMap.find i ctx.curclass.cl_statics in
		let is_impl = has_class_field_flag f CfImpl in
		let is_enum = has_class_field_flag f CfEnum in
		if is_impl && not (has_class_field_flag ctx.curfield CfImpl) && not is_enum then
			typing_error (Printf.sprintf "Cannot access non-static field %s from static method" f.cf_name) p;
		let e,fa = match ctx.curclass.cl_kind with
			| KAbstractImpl a when is_impl && not is_enum ->
				let tl = extract_param_types a.a_params in
				let e = get_this ctx p in
				let e = {e with etype = TAbstract(a,tl)} in
				e,FHAbstract(a,tl,ctx.curclass)
			| _ ->
				let e = type_type ctx ctx.curclass.cl_path p in
				e,FHStatic ctx.curclass
		in
		field_access ctx mode f fa e p
	with Not_found -> try
		(* module-level statics *)
		(match ctx.m.curmod.m_statics with
		| None -> raise Not_found
		| Some c ->
			let f = PMap.find i c.cl_statics in
			let e = type_module_type ctx (TClassDecl c) None p in
			field_access ctx mode f (FHStatic c) e p
		)
	with Not_found -> try
		let wrap e =
			let acc = AKExpr e in
			if is_set then
				AKNo(acc,p)
			else
				acc
		in
		(* lookup imported enums *)
		let rec loop l =
			match l with
			| [] -> raise Not_found
			| (t,pt) :: l ->
				match t with
				| TAbstractDecl ({a_impl = Some c} as a) when a.a_enum ->
					begin try
						let cf = PMap.find i c.cl_statics in
						if not (has_class_field_flag cf CfEnum) then
							loop l
						else begin
							let et = type_module_type ctx (TClassDecl c) None p in
							let inline = match cf.cf_kind with
								| Var {v_read = AccInline} -> true
								|  _ -> false
							in
							let fa = FieldAccess.create et cf (FHAbstract(a,extract_param_types a.a_params,c)) inline p in
							ImportHandling.mark_import_position ctx pt;
							AKField fa
						end
					with Not_found ->
						loop l
					end
				| TClassDecl _ | TAbstractDecl _ ->
					loop l
				| TTypeDecl t ->
					(match follow t.t_type with
					| TEnum (e,_) -> loop ((TEnumDecl e,pt) :: l)
					| TAbstract (a,_) when a.a_enum -> loop ((TAbstractDecl a,pt) :: l)
					| _ -> loop l)
				| TEnumDecl e ->
					try
						let ef = PMap.find i e.e_constrs in
						let et = type_module_type ctx t None p in
						ImportHandling.mark_import_position ctx pt;
						wrap (mk (TField (et,FEnum (e,ef))) (enum_field_type ctx e ef p) p)
					with
						Not_found -> loop l
		in
		(try loop (List.rev_map (fun t -> t,null_pos) ctx.m.curmod.m_types) with Not_found -> loop ctx.m.module_imports)
	with Not_found ->
		(* lookup imported globals *)
		let t, name, pi = PMap.find i ctx.m.module_globals in
		ImportHandling.mark_import_position ctx pi;
		let e = type_module_type ctx t None p in
		type_field_default_cfg ctx e name p mode with_type

and type_ident ctx i p mode with_type =
	try
		type_ident_raise ctx i p mode with_type
	with Not_found -> try
		(* lookup type *)
		if is_lower_ident i p then raise Not_found;
		let e = (try type_type ctx ([],i) p with Error (Module_not_found ([],name),_) when name = i -> raise Not_found) in
		AKExpr e
	with Not_found ->
		let resolved_to_type_parameter = ref false in
		try
			let t = List.find (fun tp -> tp.ttp_name = i) ctx.type_params in
			resolved_to_type_parameter := true;
			let c = match follow (extract_param_type t) with TInst(c,_) -> c | _ -> die "" __LOC__ in
			if TypeloadCheck.is_generic_parameter ctx c && Meta.has Meta.Const c.cl_meta then begin
				let e = type_module_type ctx (TClassDecl c) None p in
				AKExpr {e with etype = (extract_param_type t)}
			end else
				raise Not_found
		with Not_found ->
			if ctx.untyped then begin
				if i = "__this__" then
					AKExpr (mk (TConst TThis) ctx.tthis p)
				else
					let t = mk_mono() in
					AKExpr ((mk (TIdent i)) t p)
			end else begin
				if ctx.curfun = FunStatic && PMap.mem i ctx.curclass.cl_fields then typing_error ("Cannot access " ^ i ^ " in static function") p;
				if !resolved_to_type_parameter then begin
					display_error ctx.com ("Only @:const type parameters on @:generic classes can be used as value") p;
					AKExpr (mk (TConst TNull) t_dynamic p)
				end else begin
					let err = Unknown_ident i in
					if ctx.in_display then begin
						raise (Error (err,p))
					end;
					if Diagnostics.error_in_diagnostics_run ctx.com p then begin
						DisplayToplevel.handle_unresolved_identifier ctx i p false;
						DisplayFields.handle_missing_ident ctx i mode with_type p;
						let t = mk_mono() in
						AKExpr (mk (TIdent i) t p)
					end else match ctx.com.display.dms_kind with
						| DMNone ->
							raise (Error(err,p))
						| _ ->
							display_error ctx.com (error_msg err) p;
							let t = mk_mono() in
							(* Add a fake local for #8751. *)
							if !ServerConfig.legacy_completion then
								ignore(add_local ctx VGenerated i t p);
							AKExpr (mk (TIdent i) t p)
				end
			end

and handle_efield ctx e p0 mode with_type =
	let open TyperDotPath in

	let dot_path first pnext =
		let {name = name; pos = p} = first in
		try
			(* first, try to resolve the first ident in the chain and access its fields.
			   this doesn't support untyped identifiers yet, because we want to check fully-qualified
			   paths first (even in an untyped block) *)
			field_chain ctx pnext (type_ident_raise ctx name p MGet WithType.value)
		with Not_found ->
			(* first ident couldn't be resolved, it's probably a fully qualified path - resolve it *)
			let path = (first :: pnext) in
			try
				resolve_dot_path ctx path mode with_type
			with Not_found ->
				(* dot-path resolution failed, it could be an untyped field access that happens to look like a dot-path, e.g. `untyped __global__.String` *)
				try
					(* TODO: we don't really want to do full type_ident again, just the second part of it *)
					field_chain ctx pnext (type_ident ctx name p MGet WithType.value)
				with Error (Unknown_ident _,p2) as e when p = p2 ->
					try
						(* try raising a more sensible error if there was an uppercase-first (module name) part *)
						begin
							(* TODO: we should pass the actual resolution error from resolve_dot_path instead of Not_found *)
							let rec loop pack_acc first_uppercase path =
								match path with
								| {name = name; case = PLowercase} :: rest ->
									(match first_uppercase with
									| None -> loop (name :: pack_acc) None rest
									| Some (n,p) -> List.rev pack_acc, n, None, p)
								| {name = name; case = PUppercase; pos = p} :: rest ->
									(match first_uppercase with
									| None -> loop pack_acc (Some (name,p)) rest
									| Some (n,_) -> List.rev pack_acc, n, Some name, p)
								| [] ->
									(match first_uppercase with
									| None -> raise Not_found
									| Some (n,p) -> List.rev pack_acc, n, None, p)
							in
							let pack,name,sub,p = loop [] None path in
							let mpath = (pack,name) in
							if ctx.com.module_lut#mem mpath 