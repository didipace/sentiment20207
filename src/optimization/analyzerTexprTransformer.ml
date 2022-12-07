
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

open Globals
open Ast
open Type
open Common
open AnalyzerConfig
open AnalyzerTypes
open AnalyzerTypes.BasicBlock
open AnalyzerTypes.Graph
open AnalyzerTexpr
open OptimizerTexpr

(*
	Transforms an expression to a graph, and a graph back to an expression. This module relies on TexprFilter being
	run first.

	The created graph is intact and can immediately be transformed back to an expression, or used for analysis first.
*)

let rec func ctx bb tf t p =
	let g = ctx.graph in
	let create_node kind t p =
		let bb = Graph.create_node g kind t p in
		bb.bb_loop_groups <- ctx.loop_stack;
		bb
	in
	let bb_root = create_node (BKFunctionBegin tf) tf.tf_expr.etype tf.tf_expr.epos in
	let bb_exit = create_node BKFunctionEnd tf.tf_expr.etype tf.tf_expr.epos in
	add_function g tf t p bb_root;
	add_cfg_edge bb bb_root CFGFunction;
	let bb_breaks = ref [] in
	let bb_continue = ref None in
	let b_try_stack = ref [] in
	let begin_loop bb_loop_pre bb_continue' =
		let old = !bb_breaks,!bb_continue in
		bb_breaks := [];
		bb_continue := Some bb_continue';
		let id = ctx.loop_counter in
		g.g_loops <- IntMap.add id bb_loop_pre g.g_loops;
		ctx.loop_stack <- id :: ctx.loop_stack;
		bb_continue'.bb_loop_groups <- id :: bb_continue'.bb_loop_groups;
		ctx.loop_counter <- id + 1;
		(fun () ->
			let breaks = !bb_breaks in
			bb_breaks := fst old;
			bb_continue := snd old;
			ctx.loop_stack <- List.tl ctx.loop_stack;
			breaks;
		)
	in
	let begin_try b =
		b_try_stack := b :: !b_try_stack;
		(fun () ->
			b_try_stack := List.tl !b_try_stack
		)
	in
	let add_terminator bb term =
		bb.bb_terminator <- term;
		close_node bb;
		g.g_unreachable
	in
	let check_unbound_call s el =
		if s = "$ref" then begin match el with
			| [{eexpr = TLocal v}] -> add_var_flag v VCaptured
			| _ -> ()
		end;
		if is_unbound_call_that_might_have_side_effects s el then ctx.has_unbound <- true;
	in
	let no_void t p =
		if ExtType.is_void (follow t) then Error.typing_error "Cannot use Void as value" p
	in
	let push_name s =
		ctx.name_stack <- s :: ctx.name_stack;
		(fun () -> ctx.name_stack <- List.tl ctx.name_stack)
	in
	let check_ref v e = if ExtType.has_reference_semantics v.v_type then match (Texpr.skip e).eexpr with
		| TLocal v' -> add_var_flag v' VCaptured
		| _ -> ()
	in
	let rec value' bb e = match e.eexpr with
		| TLocal _ | TIdent _ ->
			bb,e
		| TBinop(OpAssign,({eexpr = TLocal v} as e1),e2) ->
			block_element bb e,e1
		| TBlock [e1] ->
			value bb e1
		| TBlock _ | TIf _ | TSwitch _ | TTry _ ->
			bind_to_temp bb false e
		| TCall({eexpr = TIdent s},el) when is_really_unbound s ->
			check_unbound_call s el;
			bb,e
		| TCall(e1,el) ->
			call bb e e1 el
		| TBinop(OpAssignOp op,({eexpr = TArray(e1,e2)} as ea),e3) ->
			array_assign_op bb op e ea e1 e2 e3
		| TBinop(OpAssignOp op,({eexpr = TField(e1,fa)} as ef),e2) ->
			field_assign_op bb op e ef e1 fa e2
		| TBinop((OpAssign | OpAssignOp _) as op,e1,e2) ->
			let bb,e1 = value bb e1 in
			let bb,e2 = value bb e2 in
			bb,{e with eexpr = TBinop(op,e1,e2)}
		| TBinop(op,e1,e2) ->
			let bb,e1,e2 = match ordered_value_list bb [e1;e2] with
				| bb,[e1;e2] -> bb,e1,e2
				| _ -> die "" __LOC__
			in
			bb,{e with eexpr = TBinop(op,e1,e2)}
		| TUnop(op,flag,e1) ->
			let bb,e1 = value bb e1 in
			bb,{e with eexpr = TUnop(op,flag,e1)}
		| TArrayDecl el ->
			let bb,el = ordered_value_list bb el in
			bb,{e with eexpr = TArrayDecl el}
		| TObjectDecl fl ->
			let el = List.map snd fl in
			let bb,el = ordered_value_list bb el in
			bb,{e with eexpr = TObjectDecl (List.map2 (fun (s,_) e -> s,e) fl el)}
		| TField({eexpr = TTypeExpr _},fa) ->
			bb,e
		| TField(e1,fa) ->
			let bb,e1 = value bb e1 in
			bb,{e with eexpr = TField(e1,fa)}
		| TArray(e1,e2) ->
			let bb,e1,e2 = match ordered_value_list bb [e1;e2] with
				| bb,[e1;e2] -> bb,e1,e2
				| _ -> die "" __LOC__
			in
			bb,{e with eexpr = TArray(e1,e2)}
		| TMeta(m,e1) ->
			let bb,e1 = value bb e1 in
			bb,{e with eexpr = TMeta(m,e1)}
		| TParenthesis e1 ->
			let bb,e1 = value bb e1 in
			bb,{e with eexpr = TParenthesis e1}
		| TCast(e1,mto) ->
			let bb,e1 = value bb e1 in
			bb,{e with eexpr = TCast(e1,mto)}
		| TNew(c,tl,el) ->
			let bb,el = ordered_value_list bb el in
			bb,{e with eexpr = TNew(c,tl,el)}
		| TEnumParameter(e1,ef,ei) ->
			let bb,e1 = value bb e1 in
			bb,{e with eexpr = TEnumParameter(e1,ef,ei)}
		| TEnumIndex e1 ->
			let bb,e1 = value bb e1 in
			bb,{e with eexpr = TEnumIndex e1}
		| TFunction tf ->
			let bb_func,bb_func_end = func ctx bb tf e.etype e.epos in
			let e_fun = mk (TConst (TString "fun")) t_dynamic p in
			let econst = mk (TConst (TInt (Int32.of_int bb_func.bb_id))) ctx.com.basic.tint e.epos in
			let ec = mk (TCall(e_fun,[econst])) t_dynamic p in
			let bb_next = create_node BKNormal bb.bb_type bb.bb_pos in
			add_cfg_edge bb bb_next CFGGoto;
			set_syntax_edge bb (SEMerge bb_next);
			close_node bb;
			add_cfg_edge bb_func_end bb_next CFGGoto;
			bb_next,ec
		| TConst _ | TTypeExpr _ ->
			bb,e
		| TThrow _ | TReturn _ | TBreak | TContinue ->
			let bb = block_element bb e in
			bb,mk (TConst TNull) t_dynamic e.epos
		| TVar _ | TFor _ | TWhile _ ->
			Error.typing_error "Cannot use this expression as value" e.epos
	and value bb e =
		let bb,e = value' bb e in
		no_void e.etype e.epos;
		bb,e
	and ordered_value_list bb el =
		let might_be_affected,collect_modified_locals = create_affection_checker() in
		let rec can_be_optimized e = match e.eexpr with
			| TBinop _ | TArray _ | TCall _ -> true
			| TParenthesis e1 -> can_be_optimized e1
			| _ -> false
		in
		let _,el = List.fold_left (fun (had_side_effect,acc) e ->
			if had_side_effect then
				(true,(might_be_affected e || has_side_effect e,can_be_optimized e,e) :: acc)
			else begin
				let had_side_effect = has_side_effect e in
				if had_side_effect then collect_modified_locals e;
				let opt = can_be_optimized e in
				(had_side_effect || opt,(false,opt,e) :: acc)
			end
		) (false,[]) (List.rev el) in
		let bb,values = List.fold_left (fun (bb,acc) (aff,opt,e) ->
			if bb == g.g_unreachable then
				bb,acc
			else begin
				let bb,value = if aff || opt then bind_to_temp bb aff e else value bb e in
				bb,(value :: acc)
			end
		) (bb,[]) el in
		bb,List.rev values
	and bind_to_temp ?(v=None) bb sequential e =
		let is_probably_not_affected e e1 fa = match fa with
			| FAnon cf | FInstance (_,_,cf) | FStatic (_,cf) | FClosure (_,cf) when cf.cf_kind = Method MethNormal -> true
			| FStatic(_,{cf_kind = Method MethDynamic}) -> false
			| FEnum _ -> true
			| FDynamic ("cca" | "__Index" | "__s") -> true (* This is quite retarded, but we have to deal with this somehow... *)
			| _ -> match follow e.etype,follow e1.etype with
				| TFun _,TInst _ -> false
				| TFun _,_ -> true (* We don't know what's going on here, don't create a temp var (see #5082). *)
				| _ -> false
		in
		let rec loop fl e = match e.eexpr with
			| TField(e1,fa) when is_probably_not_affected e e1 fa ->
				loop ((fun e' -> {e with eexpr = TField(e',fa)}) :: fl) e1
			| TField(e1,fa) ->
				let fa = match fa with
					| FInstance(c,tl,({cf_kind = Method _ } as cf)) -> FClosure(Some(c,tl),cf)
					| _ -> fa
				in
				fl,{e with eexpr = TField(e1,fa)}
			| _ ->
				fl,e
		in
		let fl,e = loop [] e in
		let rec loop e = match e.eexpr with
			| TLocal v -> v.v_name
			| TArray(e1,_) | TField(e1,_) | TParenthesis e1 | TCast(e1,None) | TMeta(_,e1) -> loop e1
			| _ -> match ctx.name_stack with
				| s :: _ -> s
				| [] -> ctx.temp_var_name
		in
		let v = match v with Some v -> v | None -> alloc_var VGenerated (loop e) e.etype e.epos in
		let bb = declare_var_and_assign bb v e e.epos in
		let e = {e with eexpr = TLocal v} in
		let e = List.fold_left (fun e f -> f e) e fl in
		bb,e
	and declare_var_and_assign bb v e p =
		no_void v.v_type p;
		(* TODO: this section shouldn't be here because it can be handled as part of the normal value processing *)
		let rec loop bb e = match e.eexpr with
			| TParenthesis e1 ->
				loop bb e1
			| TBlock el ->
				let rec loop2 bb el = match el with
					| [e] ->
						bb,e
					| e1 :: el ->
						let bb = block_element bb e1 in
						if bb == g.g_unreachable then raise Exit;
						loop2 bb el
					| [] ->
						die "" __LOC__
				in
				let bb,e = loop2 bb el in
				loop bb e
			| _ ->