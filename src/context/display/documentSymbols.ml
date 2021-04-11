open Ast
open Globals
open DisplayTypes.SymbolKind

let collect_module_symbols mname with_locals (pack,decls) =
	let l = DynArray.create() in
	let add name kind location parent deprecated =
		let si = DisplayTypes.SymbolInformation.make name kind location (if parent = "" then None else Some parent) deprecated in
		DynArray.add l si;
	in
	let rec expr parent (e,p) =
		let add name kind location = add name kind location parent in
		begin match e with
		| EVars vl ->
			List.iter (fun v ->
				add (fst v.ev_name) Variable (snd v.ev_name) false;
				expr_opt parent v.ev_expr
			) vl
		| ETry(e1,catches) ->
			expr parent e1;
			List.iter (fun ((s,p),_,e,_) ->
				add s Variable p false;
				expr parent e
			) catches;
		| EFunction(FKNamed ((s,_),_),f) ->
			add s Function p false;
			func parent f
		| EBinop(OpIn,(EConst(Ident s),p),e2) ->
			add s Variable p false;
			expr parent e2;
		| _ ->
			iter_expr (expr parent) (e,p)
		end
	and expr_opt parent eo = match eo with
		| None -> ()
		| Some e -> expr parent e
	and func parent f =
		List.iter (fun ((s,p),_,_,_,eo) ->
			add s Variable p parent false;
			expr_opt parent eo
		) f.f_args;
		expr_opt parent f.f_expr
	in
	let is_deprecated meta = Meta.has Meta.Deprecated meta in
	let field' parent parent_kind cff_name cff_kind cff_access cff_pos cff_meta =
		let field_parent = parent ^ "." ^ (fst cff_name) in
		let add_field kind = add (fst cff_name) kind cff_pos parent (is_deprecated cff_meta) in
		match cff_kind with
		| FVar(_,eo) ->
			add_field (
				if parent_kind = EnumAbstract && not (List.mem_assoc AStatic cff_access) then EnumMember
				else if (List.mem_assoc AInline cff_access) 