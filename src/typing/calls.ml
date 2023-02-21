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
			| FHStatic c ->
				(fun t -> t)
			| FHInstance(c,tl) ->
				(fun t -> t)
			| FHAbstract(a,tl,c) ->
				if a.a_enum then begin
					(* Enum abstracts have to apply their type parameters because they are basically statics with type params (#8700). *)
					let monos = Monomorph.spawn_constrained_monos (fun t -> t) a.a_params in
					apply_params a.a_params monos;
				end else
					(fun t -> t)
			| _ ->
				die "" __LOC__
		in
		ignore(follow cf.cf_type); (* force computing *)
		begin match cf.cf_kind,cf.cf_expr with
		| _ when not (ctx.com.display.dms_inline) ->
			FieldAccess.get_field_expr fa FRead
		| Method _,_->
			let chk_class c = ((has_class_flag c CExtern) || has_class_field_flag cf CfExtern) && not (Meta.has Meta.Runtime cf.cf_meta) in
			let wrap_extern c =
				let c2 =
					let m = c.cl_module in
					let mpath = (fst m.m_path @ ["_" ^ snd m.m_path],(snd m.m_path) ^ "_Impl_") in
					try
						let rec loop mtl = match mtl with
							| (TClassDecl c) :: _ when c.cl_path = mpath -> c
							| _ :: mtl -> loop mtl
							| [] -> raise Not_found
						in
						loop c.cl_module.m_types
					with Not_found ->
						let c2 = mk_class c.cl_module mpath c.cl_pos null_pos in
						c.cl_module.m_types <- (TClassDecl c2) :: c.cl_module.m_types;
						c2
				in
				let cf = try
					PMap.find cf.cf_name c2.cl_statics
				with Not_found ->
					let cf = {cf with cf_kind = Method MethNormal} in
					c2.cl_statics <- PMap.add cf.cf_name cf c2.cl_statics;
					c2.cl_ordered_statics <- cf :: c2.cl_ordered_statics;
					cf
				in
				let e_t = type_module_type ctx (TClassDecl c2) None p in
				FieldAccess.get_field_expr (FieldAccess.create e_t cf (FHStatic c2) true p) FRead
			in
			let e_def = FieldAccess.get_field_expr fa FRead in
			begin match follow fa.fa_on.etype with
				| TInst (c,_) when chk_class c ->
					display_error ctx.com "Can't create closure on an extern inline member method" p;
					e_def
				| TAnon a ->
					begin match !(a.a_status) with
						| Statics c when has_class_field_flag cf CfExtern ->
							display_error ctx.com "Cannot create closure on @:extern inline method" p;
							e_def
						| Statics c when chk_class c -> wrap_extern c
						| _ -> e_def
					end
				| _ -> e_def
			end
		| Var _,Some e ->
			let rec loop e = Type.map_expr loop { e with epos = p; etype = apply_params e.etype } in
			let e = loop e in
			let e = Inline.inline_metadata e cf.cf_meta in
			let tf = apply_params cf.cf_type in
			if not (type_iseq tf e.etype) then mk (TCast(e,None)) tf e.epos
			else e
		| Var _,None ->
			typing_error "Recursive inline is not supported" p
		end
	in
	let dispatcher p = new call_dispatcher ctx MGet WithType.value p in
	match g with
	| AKNo(_,p) -> typing_error ("This expression cannot be accessed for reading") p
	| AKExpr e -> e
	| AKSafeNav sn ->
		(* generate null-check branching for the safe navigation chain *)
		let eobj = sn.sn_base in
		let enull = Builder.make_null eobj.etype sn.sn_pos in
		let eneq = Builder.binop OpNotEq eobj enull ctx.t.tbool sn.sn_pos in
		let ethen = acc_get