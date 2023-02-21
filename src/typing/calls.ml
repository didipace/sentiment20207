open Globals
open DisplayTypes.DisplayMode
open Common
open Ast
open Type
open Typecore
open TyperBase
open Fields
open Error
open CallUnification

let make_call ctx e params t ?(force_inline=false) p =
	let params =
		match follow e.etype with
		| TFun (expected_args,_) ->
			(match List.rev expected_args with
			| (_,true,t) :: rest when is_pos_infos t && List.length rest = List.length params ->
				let infos = mk_infos ctx p [] in
				params @ [type_expr ctx infos (WithType.with_type t)]
			| _ -> params
			)
		| _ -> params
	in
	try
		let ethis,cl,f = match e.eexpr with
			| TField (ethis,fa) ->
				let co,cf = match fa with
					| FInstance(c,_,cf) | FStatic(c,cf) -> Some c,cf
					| FAnon cf -> None,cf
					| _ -> raise Exit
				in
				ethis,co,cf
			| _ ->
				raise Exit
		in
		if not force_inline then begin
			let is_extern_class = match cl with Some c -> (has_class_flag c CExtern) | _ -> false in
			if not (Inline.needs_inline ctx is_extern_class f) then raise Exit;
		end else begin
			match cl with
			| None ->
				()
			| Some c ->
				(* Delay this to filters because that's when cl_descendants is set. *)
				ctx.com.callbacks#add_before_save (fun () ->
					let rec has_override c =
						PMap.mem f.cf_name c.cl_fields
						|| List.exists has_override c.cl_descendants
					in
					if List.exists has_override c.cl_descendants then typing_error (Printf.sprintf "Cannot force inline-call to %s because it is overridden" f.cf_name) p
				)
		end;
		let config = Inline.inline_config cl f params t in
		ignore(follow f.cf_type); (* force evaluation *)
		(match cl, ctx.curclass.cl_kind, params with
			| Some c, KAbstractImpl _, { eexpr = TLocal { v_meta = v_meta } } :: _ when c == ctx.curclass ->
				if
					f.cf_name <> "_new"
					&& has_meta Meta.This v_meta
					&& has_class_field_flag f CfModifiesThis
				then
					if assign_to_this_is_allowed ctx then
						(* Current method needs to infer CfModifiesThis flag, since we are calling a method, which modifies `this` *)
						add_class_field_flag ctx.curfield CfModifiesThis
					else
						typing_error ("Abstract 'this' value can only be modified inside an inline function. '" ^ f.cf_name ^ "' modifies 'this'") p;
			| _ -> ()
		);
		let params = List.map (Optimizer.reduce_expression ctx) params in
		let force_inline = is_forced_inline cl f in
		(match f.cf_expr_unoptimized,f.cf_expr with
		| Some {eexpr = TFunction fd},_
		| None,Some { eexpr = TFunction fd } ->
			(match Inline.type_inline ctx f fd ethis params t config p force_inline with
			| None ->
				if force_inline then typing_error "Inline could not be done" p;
				raise Exit;
			| Some e -> e)
		| _ ->
			(*
				we can't inline because there is most likely a loop in the typing.
				this can be caused by mutually recursive vars/functions, some of them
				being inlined or not. In that case simply ignore inlining.
			*)
			raise Exit)
	with Exit ->
		mk (TCall (e,params)) t p

let mk_array_get_call ctx (cf,tf,r,e1) c ebase p = match cf.cf_expr with
	| None when not (has_class_field_flag cf CfExtern) ->
		if not (Meta.has Meta.NoExpr cf.cf_meta) then display_error ctx.com "Recursive array get method" p;
		mk (TArray(ebase,e1)) r p
	| _ ->
		let et = type_module_type ctx (TClassDecl c) None p in
		let ef = mk (TField(et,(FStatic(c,cf)))) tf p in
		make_call ctx ef [ebase;e1] r p

let mk_array_set_call ctx (cf,tf,r,e1,evalue) c ebase p =
	match cf.cf_expr with
		| None when not (has_class_field_flag cf CfExtern) ->
			if not (Meta.has Meta.NoExpr cf.cf_meta) then display_error ctx.com "Recursive array set method" p;
			let ea = mk (TArray(ebase,e1)) r p in
			mk (TBinop(OpAssign,ea,evalue)) r p
		| _ ->
			let et = type_module_type ctx (TClassDecl c) None p in
			let ef = mk (TField(et,(FStatic(c,cf)))) tf p in
			make_call ctx ef [ebase;e1;evalue] r p

let abstract_using_param_type sea = match follow sea.se_this.etype with
	| TAbstract(a,tl) when has_class_field_flag sea.se_access.fa_field CfImpl -> apply_params a.a_params tl a.a_this
	| _ -> sea.se_this.etype

let rec acc_get ctx g =
	let inline_read fa =
		let cf = fa.fa_field in
		let p = fa.fa_pos in
		(* do not create a closure for static calls *)
		let apply_params = match fa.fa_host with
			| FHStatic c 