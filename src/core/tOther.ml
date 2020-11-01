open Globals
open Ast
open TType
open TFunctions
open TPrinting

module TExprToExpr = struct
	let tpath p mp pl =
		if snd mp = snd p then
			CTPath (mk_type_path ~params:pl p)
		else
			CTPath (mk_type_path ~params:pl ~sub:(snd p) mp)

	let rec convert_type = function
		| TMono r ->
			(match r.tm_type with
			| None -> raise Exit
			| Some t -> convert_type t)
		| TInst ({cl_private = true; cl_path=_,name},tl)
		| TEnum ({e_private = true; e_path=_,name},tl)
		| TType ({t_private = true; t_path=_,name},tl)
		| TAbstract ({a_private = true; a_path=_,name},tl) ->
			CTPath (mk_type_path ~params:(List.map tparam tl) ([],name))
		| TEnum (e,pl) ->
			tpath e.e_path e.e_module.m_path (List.map tparam pl)
		| TInst({cl_kind = KExpr e} as c,pl) ->
			tpath ([],snd c.cl_path) ([],snd c.cl_path) (List.map tparam pl)
		| TInst({cl_kind = KTypeParameter _} as c,pl) ->
			tpath ([],snd c.cl_path) ([],snd c.cl_path) (List.map tparam pl)
		| TInst (c,pl) ->
			tpath c.cl_path c.cl_module.m_path (List.map tparam pl)
		| TType (t,pl) as tf ->
			(* recurse on type-type *)
			if (snd t.t_path).[0] = '#' then convert_type (follow tf) else tpath t.t_path t.t_module.m_path (List.map tparam pl)
		| TAbstract (a,pl) ->
			tpath a.a_path a.a_module.m_path (List.map tparam pl)
		| TFun (args,ret) ->
			CTFunction (List.map (fun (n,o,t) ->
				let ct = convert_type' t in
					let ct = if n = "" then ct else CTNamed((n,null_pos),ct),null_pos in
					if o then CTOptional ct,null_pos else ct
				) args, (convert_type' ret))
		| TAnon a ->
			begin match !(a.a_status) with
			| Statics c -> tpath ([],"Class") ([],"Class") [TPType (tpath c.cl_path c.cl_path [],null_pos)]
			| EnumStatics e -> tpath ([],"Enum") ([],"Enum") [TPType (tpath e.e_path e.e_path [],null_pos)]
			| _ ->
				CTAnonymous (PMap.foldi (fun _ f acc ->
					let access = ref [] in
					let add flag =
						access := (flag,null_pos) :: !access;
					in
					if has_class_field_flag f CfPublic then add APublic else add APrivate;
					if has_class_field_flag f CfFinal then add AFinal;
					if has_class_field_flag f CfExtern then add AExtern;
					let kind = match (f.cf_kind,follow f.cf_type) with
						| (Var v,ret) ->
							let var_access_to_string va get_or_set = match va with
								| AccNormal | AccCtor | AccInline | AccRequire _ -> "default"
								| AccNo -> "null"
								| AccNever -> "never"
								| AccCall -> get_or_set
							in
							let read = (var_access_to_string v.v_read "get",null_pos) in
							let write = (var_access_to_string v.v_write "set",null_pos) in
							FProp (read,write,mk_type_hint f.cf_type null_pos,None)
						| Method _,TFun(args,ret) ->
							FFun({
								f_params = [];
								f_args = List.map (fun (n,o,t) ->
									((n,null_pos),o,[],Some (convert_type t,null_pos),None)
								) args;
								f_type = Some (convert_type ret,null_pos);
								f_expr = None;
							})
						| _ ->
							die "" __LOC__
					in
					{
						cff_name = f.cf_name,null_pos;
						cff_kind = kind;
						cff_pos = f.cf_pos;
						cff_doc = f.cf_doc;
						cff_meta = f.cf_meta;
						cff_access = !access;
					} :: acc
				) a.a_fields [])
			end
		| (TDynamic t2) as t ->
			tpath ([],"Dynamic") ([],"Dynamic") (if t == t_dynamic then [] else [tparam t2])
		| TLazy f ->
			convert_type (lazy_type f)

	and convert_type' t =
		convert_type t,null_pos

	and tparam = function
		| TInst ({cl_kind = KExpr e}, _) -> TPExpr e
		| t -> TPType (convert_type' t)

	and mk_type_hint t p =
		match follow t with
		| TMono _ -> None
		| _ -> (try Some (convert_type t,p) with Exit -> None)

	let rec convert_expr e =
		let full_type_path t =
			let mp,p = match t with
			| TClassDecl c -> c.cl_module.m_path,c.cl_path
			| TEnumDecl en -> en.e_module.m_path,en.e_path
			| TAbstractDecl a -> a.a_module.m_path,a.a_path
			| TTypeDecl t -> t.t_module.m_path,t.t_path
			in
			if snd mp = snd p then p else (fst mp) @ [snd mp],snd p
		in
		let mk_path = expr_of_type_path in
		let mk_ident = function
			| "`trace" -> Ident "trace"
			| n -> Ident n
		in
		let eopt = function None -> None | Some e -> Some (convert_expr e) in
		((match e.eexpr with
		| TConst c ->
			EConst (tconst_to_const c)
		| TLocal v -> EConst (mk_ident v.v_name)
		| TArray (e1,e2) -> EArray (convert_expr e1,convert_expr e2)
		| TBinop (op,e1,e2) -> EBinop (op, convert_expr e1, convert_expr e2)
		| TField (e,f) -> EField (convert_expr e, field_name f, EFNormal)
		| TTypeExpr t -> fst (mk_path (full_type_path t) e.epos)
		| TParenthesis e -> EParenthesis (convert_expr e)
		| TObjectDecl fl -> EObjectDecl (List.map (fun (k,e) -> k, convert_expr e) fl)
		| TArrayDecl el -> EArrayDecl (List.map convert_expr el)
		| TCall (e,el) -> ECall (convert_expr e,List.map convert_expr el)
		| TNew (c,pl,el) -> ENew ((match (try convert_type (TInst (c,pl)) with Exit -> convert_type (TInst (c,[]))) with CTPath p -> p,null_pos | _ -> die "" __LOC__),List.map convert_expr el)
		| TUnop (op,p,e) -> EUnop (op,p,convert_expr e)
		| TFunction f ->
			let arg (v,c) = (v.v_name,v.v_pos), false, v.v_meta, mk_type_hint v.v_type null_pos, (match c with None -> None | Some c -> Some (convert_expr c)) in
			EFunction (FKAnonymous,{ f_params = []; f_args = List.map arg f.tf_args; f_type = mk_type_hint f.tf_type null_pos; f_expr = Some (convert_expr f.tf_expr) })
		| TVar (v,eo) ->
			let final = has_var_flag v VFinal
			and t = mk_type_hint v.v_type v.v_pos
			and eo = eopt eo in
			EVars ([mk_evar ~final ?