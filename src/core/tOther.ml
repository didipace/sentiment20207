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
