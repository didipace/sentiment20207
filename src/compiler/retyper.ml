
open Globals
open Ast
open Typecore
open Type
open TypeloadModule
open TypeloadFields

exception Fail of string

type retyping_context = {
	typer : typer;
	print_stack : string list;
}

let fail rctx s =
	let stack = String.concat " " (List.rev rctx.print_stack) in
	raise (Fail (Printf.sprintf "%s: %s" stack s))

let disable_typeloading rctx ctx f =
	let old = ctx.g.load_only_cached_modules in
	ctx.g.load_only_cached_modules <- true;
	try
		Std.finally (fun () -> ctx.g.load_only_cached_modules <- old) f ()
	with (Error.Error (Module_not_found path,_)) ->
		fail rctx (Printf.sprintf "Could not load [Module %s]" (s_type_path path))

let pair_type th t = match th with
	| None ->
		TExprToExpr.convert_type t,null_pos
	| Some t ->
		t

let pair_class_field rctx ctx cctx fctx cf cff p =
	match cff.cff_kind with
	| FFun fd ->
		let targs,tret = match follow cf.cf_type with
			| TFun(args,ret) ->
				args,ret
			| _ ->
				fail rctx "Type change"
		in
		let args = try
			List.map2 (fun (name,opt,meta,th,eo) (_,_,t) ->
				(name,opt,meta,Some (pair_type th t),eo)
			) fd.f_args targs
		with Invalid_argument _ ->
			fail rctx "Type change"
		in
		let ret = pair_type fd.f_type tret in
		let fd = {
			fd with
			f_args = args;
			f_type = Some ret
		} in
		let load_args_ret () =
			setup_args_ret ctx cctx fctx (fst cff.cff_name) fd p
		in
		let args,ret = disable_typeloading rctx ctx load_args_ret in
		let t = TFun(args#for_type,ret) in
		(fun () ->
			(* This is the only part that should actually modify anything. *)
			cf.cf_type <- t;
			TypeBinding.bind_method ctx cctx fctx cf t args ret fd.f_expr (match fd.f_expr with Some e -> snd e | None -> cff.cff_pos);
			if ctx.com.display.dms_full_typing then
				remove_class_field_flag cf CfPostProcessed;
		)
	| FVar(th,eo) | FProp(_,_,th,eo) ->
		let th = Some (pair_type th cf.cf_type) in
		let t = disable_typeloading rctx ctx (fun () -> load_variable_type_hint ctx fctx eo (pos cff.cff_name) th) in
		(fun () ->
			cf.cf_type <- t;
			TypeBinding.bind_var ctx cctx fctx cf eo;
			if ctx.com.display.dms_full_typing then
				remove_class_field_flag cf CfPostProcessed;
		)

let pair_classes rctx context_init c d p =
	let rctx = {rctx with
		print_stack = (Printf.sprintf "[Class %s]" (s_type_path c.cl_path)) :: rctx.print_stack
	} in
	c.cl_restore();
	(* TODO: What do we do with build macros? *)
	let cctx = create_class_context c context_init p in
	let ctx = create_typer_context_for_class rctx.typer cctx p in
	let _ =
		let rctx = {rctx with
			print_stack = (Printf.sprintf "[Relations]") :: rctx.print_stack
		} in
		let has_extends = ref false in
		let implements = ref c.cl_implements in
		List.iter (function
			| HExtends(path,p) ->
				has_extends := true;
				begin match c.cl_super with
				| None ->
					fail rctx (Printf.sprintf "parent %s appeared" (Ast.Printer.s_complex_type_path "" (path,p)))
				| Some(c,tl) ->
					let th = pair_type (Some(CTPath path,p)) (TInst(c,tl)) in
					ignore (disable_typeloading rctx ctx (fun () -> Typeload.load_complex_type ctx false th))
				end
			| HImplements(path,p) ->
				begin match !implements with
					| (c,tl) :: rest ->
						(* TODO: I think this should somehow check if it's actually the same interface. There could be cases
						   where the order changes or something like that... Maybe we can compare the loaded type.
						   However, this doesn't matter until we start retyping invalidated modules.
						*)
						implements := rest;
						let th = pair_type (Some(CTPath path,p)) (TInst(c,tl)) in
						ignore (disable_typeloading rctx ctx (fun () -> Typeload.load_complex_type ctx false th));
					| [] ->
						fail rctx (Printf.sprintf "interface %s appeared" (Ast.Printer.s_complex_type_path "" (path,p)))
				end
			| _ ->
				()
		) d.d_flags;
		(* TODO: There are probably cases where the compiler generates a cl_super even though it's not in syntax *)
		if not !has_extends then begin match c.cl_super with
			| None -> ()
			| Some(c,_) -> fail rctx (Printf.sprintf "parent %s disappeared" (s_type_path c.cl_path))
		end;
		begin match !implements with
			| (c,_) :: _ -> fail rctx (Printf.sprintf "interface %s disappeared" (s_type_path c.cl_path))
			| [] -> ()
		end
	in