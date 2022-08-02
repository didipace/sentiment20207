open Globals
open Ast
open Type
open Typecore
open Error

type access_kind =
	(* Access is not possible or allowed. *)
	| AKNo of access_kind * pos
	(* Access on arbitrary expression. *)
	| AKExpr of texpr
	(* Safe navigation access chain *)
	| AKSafeNav of safe_nav_access
	(* Access on non-property field. *)
	| AKField of field_access
	(* Access on property field. The field is the property, not the accessor. *)
	| AKAccessor of field_access
	(* Access via static extension. *)
	| AKUsingField of static_extension_access
	(* Access via static extension on property field. The field is the property, not the accessor.
	   This currently only happens on abstract properties. *)
	| AKUsingAccessor of static_extension_access
	(* Access on abstract via array overload. *)
	| AKAccess of tabstract * tparams * tclass * texpr * texpr
	(* Access on abstract via resolve method. *)
	| AKResolve of static_extension_access * string

and safe_nav_access = {
	(* position of the safe navigation chain start (the initial ?.field expression) *)
	sn_pos : pos;
	(* starting value to be checked for null *)
	sn_base : texpr;
	(* temp var declaration to store complex base expression *)
	sn_temp_var : texpr option;
	(* safe navigation access to be done if the base value is not null *)
	sn_access : access_kind;
}

type object_decl_kind =
	| ODKWithStructure of tanon
	| ODKWithClass of tclass * tparams
	| ODKPlain
	| ODKFailed

let type_call_target_ref : (typer -> expr -> expr list -> WithType.t -> pos option -> access_kind) ref = ref (fun _ _ _ _ -> die "" __LOC__)
let type_access_ref : (typer -> expr_def -> pos -> access_mode -> WithType.t -> access_kind) ref = ref (fun _ _ _ _ _ -> assert false)

class value_reference (ctx : typer) =

object(self)
	val vars = DynArray.create ()

	method get_vars = DynArray.to_list vars

	method as_var name e =
		let v = alloc_var VGenerated name e.etype e.epos in
		DynArray.add vars (v,e);
		mk (TLocal v) v.v_type v.v_pos

	method private get_expr_aux depth name e =
		let rec loop depth name e = match (Texpr.skip e).eexpr with
			| TLocal _ | TTypeExpr _ | TConst _ ->
				e
			| TField(ef,fa) when depth = 0 ->
				let ef = loop (depth + 1) "fh" ef in
				{e with eexpr = TField(ef,fa)}
			| TArray(e1,e2) when depth = 0 ->
				let e1 = loop (depth + 1) "base" e1 in
				let e2 = loop (depth + 1) "index" e2 in
				{e with eexpr = TArray(e1,e2)}
			| _ ->
				self#as_var name e
		in
		loop depth name e

	method get_expr name e =
		self#get_expr_aux 0 name e

	method get_expr_part name e =
		self#get_expr_aux 1 name e

	method to_texpr e =
		begin match self#get_vars with
		| [] ->
			e
		| vl ->
			let el = List.map (fun (v,e) ->
				mk (TVar(v,Some e)) ctx.t.tvoid v.v_pos
			) vl in
			let e = mk (TBlock (el @ [e])) e.etype e.epos in
			{e with eexpr = TMeta((Meta.MergeBlock,[],null_pos),e)}
		end

	method to_texpr_el el e =
		let vl = self#get_vars in
		let el_vars = List.map (fun (v,e) ->
			mk (TVar(v,Some e)) ctx.t.tvoid v.v_pos
		) vl in
		let e = mk (TBlock (el_vars @ el @ [e])) e.etype e.epos in
		{e with eexpr = TMeta((Meta.MergeBlock,[],null_pos),e)}
end

let is_lower_ident s p =
	try Ast.is_lower_ident s
	with Invalid_argument msg -> typing_error msg p

let get_this ctx p =
	match ctx.curfun with
	| FunStatic ->
		typing_error "Cannot access this from a static function" p
	| FunMemberClassLocal | FunMemberAbstractLocal ->
		let v = match ctx.vthis with
			| None ->
				let v = if ctx.curfun = FunMemberAbstractLocal then
					PMap.find "this" ctx.locals
				else
					add_local ctx VGenerated "`this" ctx.tthis p
				in
				ctx.vthis <- Some v;
				v
			| Some v ->
				ctx.locals <- PMap.add v.v_name v ctx.locals;
				v
		in
		mk (TLocal v) ctx.tthis p
	| FunMemberAbstract ->
		let v = (try PMap.find "this" ctx.locals with Not_found -> typing_error "Cannot reference this abstract here" p) in
		mk (TLocal v) v.v_type p
	| FunConstructor | FunMember ->
		mk (TConst TThis) ctx.tthis p

let assign_to_this_is_allowed ctx =
	match ctx.curclass.cl_kind with
		| KAbstractImpl _ ->
			(match ctx.curfield.cf_kind with
				| Method MethInline -> true
				| Method _ when ctx.curfield.cf_name = "_new" -> true
				| _ -> false
			)
		| _ -> false

let rec type_module_type ctx t tparams p =
	match t with
	| TClassDecl {cl_kind = KGenericBuild _} ->
		let _,_,f = InstanceBuilder.build_instance ctx t p in
		let t = f (match tparams with None -> [] | Some tl -> tl) in
		let mt = try
			module_type_of_type t
		with Exit ->
			if follow t == t_dynamic then Typeload.load_type_def ctx p (mk_type_path ([],"Dynamic"))
			else typing_error "Invalid module type" p
		in
		type_module_type ctx mt None p
	| TClassDecl c ->
		let t_tmp = class_module_type c in
		mk (TTypeExpr (TClassDecl c)) (TType (t_tmp,[])) p
	| TEnumDecl e ->
		let types = (match tparams with None -> Monomorph.spawn_constrained_monos (fun t -> t) e.e_params | Some l -> l) in
		mk (TTypeExpr (TEnumDecl e)) (TType (e.e_type,types)) p
	| TTypeDecl s ->
		let t = apply_typedef s (List.map (fun _ -> spawn_monomorph ctx p) s.t_params) in
		DeprecationCheck.check_typedef ctx.com s p;
		(match follow t with
		| TEnum (e,params) ->
			type_module_type ctx (TEnumDecl e) (Some params) p
		| TInst (c,params) ->
			type_module_type ctx (TClassDecl c) (Some params) p
		| TAbstract (a,params) ->
			type_module_type ctx (TAbstractDecl a) (Some params) p
		| _ ->
			typing_error (s_type_path s.t_path ^ " is not a value") p)
	| TAbstractDecl { a_impl = Some c } ->
		type_module_type ctx (TClassDecl c) tparams p
	| TAbstractDecl a ->
		if not (Meta.has Meta.RuntimeValue a.a_meta) then typing_error (s_type_path a.a_path ^ " is not a value") p;
		let t_tmp = abstract_module_type a [] in
		mk (TTypeExpr (TAbstractDecl a)) (TType (t_tmp,[])) p

let type_type ctx tpath p =
	type_module_type ctx (Typeload.load_type_def ctx p (mk_type_path tpath)) None p

let mk_module_type_access ctx t p =
	AKExpr (type_module_type ctx t None p)

let s_field_access tabs fa =
	let st = s_type (print_context()) in
	let se = s_expr_pretty true "" false st in
	let sfa = function
		| FHStatic c -> Printf.sprintf "FHStatic(%s)" (s_type_path c.cl_path)
		| FHInstance(c,tl) -> Printf.sprintf "FHInstance(%s, %s)" (s_type_path c.cl_path) (s_types tl)
		| FHAbstract(a,tl,c) -> Printf.sprintf "FHAbstract(%s, %s, %s)" (s_type_path a.a_path) (s_types tl) (s_type_path c.cl_path)
		| FHAnon -> Printf.sprintf "FHAnon"
	in
	