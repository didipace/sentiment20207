
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
open Globals
open JData
open Unix
open Ast
open Common
open Type
open Codegen
open Gencommon
open Gencommon.SourceWriter
open Printf
open Option
open ExtString

let is_boxed_type t = match follow t with
	| TInst ({ cl_path = (["java";"lang"], "Boolean") }, [])
	| TInst ({ cl_path = (["java";"lang"], "Double") }, [])
	| TInst ({ cl_path = (["java";"lang"], "Integer") }, [])
	| TInst ({ cl_path = (["java";"lang"], "Byte") }, [])
	| TInst ({ cl_path = (["java";"lang"], "Short") }, [])
	| TInst ({ cl_path = (["java";"lang"], "Character") }, [])
	| TInst ({ cl_path = (["java";"lang"], "Float") }, [])
	| TInst ({ cl_path = (["java";"lang"], "Long") }, []) -> true
	| _ -> false

let is_boxed_of_t boxed_t orig_t = match follow boxed_t, follow orig_t with
	| TInst ({ cl_path = (["java";"lang"], "Boolean") }, []),
	  TAbstract({ a_path = ([],"Bool") }, [])
	| TInst ({ cl_path = (["java";"lang"], "Double") }, []),
	  TAbstract({ a_path = ([],"Float") }, [])
	| TInst ({ cl_path = (["java";"lang"], "Integer") }, []),
	  TAbstract({ a_path = ([],"Int") }, [])
	| TInst ({ cl_path = (["java";"lang"], "Byte") }, []),
	  TAbstract({ a_path = (["java"],"Int8") }, [])
	| TInst ({ cl_path = (["java";"lang"], "Short") }, []),
	  TAbstract({ a_path = (["java"],"Int16") }, [])
	| TInst ({ cl_path = (["java";"lang"], "Character") }, []),
	  TAbstract({ a_path = (["java"],"Char16") }, [])
	| TInst ({ cl_path = (["java";"lang"], "Float") }, []),
	  TAbstract({ a_path = ([],"Single") }, [])
	| TInst ({ cl_path = (["java";"lang"], "Long") }, []),
	  TAbstract({ a_path = (["java"],"Int64") }, []) ->
		true
	| _ -> false

let is_boxed_int_type boxed_t = match follow boxed_t with
	| TInst ({ cl_path = (["java";"lang"], ("Integer"|"Byte"|"Short"|"Long")) }, []) ->
		true
	| _ -> false

let is_int64_type t = match follow t with
	| TAbstract ({ a_path = (["java"], ("Int64")) }, []) ->
		true
	| _ -> false

let is_boxed_int64_type t = match follow t with
	| TInst ({ cl_path = (["java";"lang"], ("Long")) }, []) ->
		true
	| _ -> false

let is_int_type t = match follow t with
	| TAbstract ({ a_path = ([], ("Int")) }, [])
	| TAbstract ({ a_path = (["java"], ("Int8" | "Int16" | "Int64")) }, []) ->
		true
	| _ -> false

let is_boxed_float_type boxed_t = match follow boxed_t with
	| TInst ({ cl_path = (["java";"lang"], ("Double"|"Float")) }, []) ->
		true
	| _ -> false

let is_float_type t = match follow t with
	| TAbstract ({ a_path = ([], ("Float"|"Single")) }, []) ->
		true
	| _ -> false

let is_boxed_number t = match follow t with
	| TInst ({ cl_path = (["java";"lang"], ("Float"|"Double"|"Integer"|"Byte"|"Short"|"Long")) }, []) ->
		true
	| _ ->
		false

let is_unboxed_number t = match follow t with
	| TAbstract ({ a_path = ([], ("Float"|"Single"|"Int")) }, [])
	| TAbstract ({ a_path = (["java"], ("Int8" | "Int16" | "Int64")) }, []) ->
		true
	| _ -> false

let rec t_has_type_param t = match follow t with
	| TInst({ cl_kind = KTypeParameter _ }, []) -> true
	| TEnum(_, params)
	| TAbstract(_, params)
	| TInst(_, params) -> List.exists t_has_type_param params
	| TFun(f,ret) -> t_has_type_param ret || List.exists (fun (_,_,t) -> t_has_type_param t) f
	| _ -> false

let is_dynamic gen t =
	match follow (gen.greal_type t) with
		| TDynamic _ -> true
		| _ -> false

let is_type_param t = ExtType.is_type_param (follow t)

let rec t_has_type_param_shallow last t = match follow t with
	| TInst({ cl_kind = KTypeParameter _ }, []) -> true
	| TEnum(_, params)
	| TAbstract(_, params)
	| TInst(_, params) when not last -> List.exists (t_has_type_param_shallow true) params
	| TFun(f,ret) when not last -> t_has_type_param_shallow true ret	|| List.exists (fun (_,_,t) -> t_has_type_param_shallow true t) f
	| _ -> false

let rec replace_type_param t = match follow t with
	| TInst({ cl_kind = KTypeParameter _ }, []) -> t_dynamic
	| TEnum(e, params) -> TEnum(e, List.map replace_type_param params)
	| TAbstract(a, params) -> TAbstract(a, List.map replace_type_param params)
	| TInst(cl, params) -> TInst(cl, List.map replace_type_param params)
	| _ -> t

let in_runtime_class gen =
	match gen.gcurrent_class with
	| Some { cl_path = ["haxe";"lang"],"Runtime"} -> true
	| _ -> false

let is_java_basic_type t =
	match follow t with
		| TInst( { cl_path = (["haxe"], "Int32") }, [] )
		| TInst( { cl_path = (["haxe"], "Int64") }, [] )
		| TAbstract( { a_path = (["java"], ("Int8" | "Int16" | "Char16" | "Int64")) }, [] )
		| TAbstract( { a_path =	([], ("Int"|"Float"|"Bool"|"Single")) }, [] ) ->
			true
		| _ -> false

let is_bool t =
	match follow t with
		| TAbstract ({ a_path = ([], "Bool") },[]) ->
			true
		| _ -> false

let like_bool t =
	match follow t with
		| TAbstract ({ a_path = ([], "Bool") },[])
		| TAbstract ({ a_path = (["java";"lang"],"Boolean") },[])
		| TInst ({ cl_path = (["java";"lang"],"Boolean") },[]) ->
			true
		| _ -> false

let is_int_float gen t =
	match follow (gen.greal_type t) with
		| TInst( { cl_path = (["haxe"], "Int32") }, [] )
		| TAbstract( { a_path =	([], "Int") }, [] )
		| TAbstract( { a_path =	([], "Float") }, [] ) ->
			true
		| (TAbstract _ as t) when like_float t && not (like_i64 t)-> true
		| _ -> false

let parse_explicit_iface =
	let regex = Str.regexp "\\." in
	let parse_explicit_iface str =
		let split = Str.split regex str in
		let rec get_iface split pack =
			match split with
				| clname :: fn_name :: [] -> fn_name, (List.rev pack, clname)
				| pack_piece :: tl -> get_iface tl (pack_piece :: pack)
				| _ -> die "" __LOC__
		in
		get_iface split []
	in parse_explicit_iface

let is_cl t = match follow t with
	| TInst({ cl_path = ["java";"lang"],"Class" },_)
	| TAbstract({ a_path = [], ("Class"|"Enum") },_) -> true
	| TAnon(a) when is_some (anon_class t) -> true
	| _ -> false

let mk_cast_if_needed t_to e =
	if type_iseq t_to e.etype then
		e
	else
		mk_cast t_to e

(* ******************************************* *)
(* JavaSpecificESynf *)
(* ******************************************* *)
(*
	Some Java-specific syntax filters that must run before ExpressionUnwrap

	dependencies:
		It must run before ExprUnwrap, as it may not return valid Expr/Statement expressions
		It must run before ClassInstance, as it will detect expressions that need unchanged TTypeExpr
		It must run after CastDetect, as it changes casts
		It must run after TryCatchWrapper, to change Std.is() calls inside there
*)
module JavaSpecificESynf =
struct
	let name = "java_specific_e"
	let priority = solve_deps name [ DBefore ExpressionUnwrap.priority; DBefore ClassInstance.priority; DAfter CastDetect.priority]

	let get_cl_from_t t =
		match follow t with
			| TInst(cl,_) -> cl
			| _ -> die "" __LOC__

	let configure gen runtime_cl =
		let basic = gen.gcon.basic in
		let float_cl = get_cl ( get_type gen (["java";"lang"], "Double")) in
		let i8_md  = ( get_type gen (["java";"lang"], "Byte")) in
		let i16_md	= ( get_type gen (["java";"lang"], "Short")) in
		let i64_md	= ( get_type gen (["java";"lang"], "Long")) in
		let c16_md	= ( get_type gen (["java";"lang"], "Character")) in
		let f_md	= ( get_type gen (["java";"lang"], "Float")) in
		let bool_md = get_type gen (["java";"lang"], "Boolean") in

		let rec run e =
			match e.eexpr with
				(* Math changes *)
				| TField( _, FStatic({ cl_path = (["java";"lang"], "Math") }, { cf_name = "NaN" }) ) ->
					mk_static_field_access_infer float_cl "NaN" e.epos []
				| TField( _, FStatic({ cl_path = (["java";"lang"], "Math") }, { cf_name = "NEGATIVE_INFINITY" }) ) ->
					mk_static_field_access_infer float_cl "NEGATIVE_INFINITY" e.epos []
				| TField( _, FStatic({ cl_path = (["java";"lang"], "Math") }, { cf_name = "POSITIVE_INFINITY" }) ) ->
					mk_static_field_access_infer float_cl "POSITIVE_INFINITY" e.epos []
				| TField( _, FStatic({ cl_path = (["java";"lang"], "Math") }, { cf_name = "isNaN"}) ) ->
					mk_static_field_access_infer float_cl "isNaN" e.epos []
				| TCall( ({ eexpr = TField( (_ as ef), FStatic({ cl_path = (["java";"lang"], "Math") }, { cf_name = ("ffloor" as f) }) ) } as fe), p)
				| TCall( ({ eexpr = TField( (_ as ef), FStatic({ cl_path = (["java";"lang"], "Math") }, { cf_name = ("fceil" as f) }) ) } as fe), p) ->
						Type.map_expr run { e with eexpr = TCall({ fe with eexpr = TField(ef, FDynamic (String.sub f 1 (String.length f - 1)))	}, p) }
				| TCall( { eexpr = TField( _, FStatic({ cl_path = (["java";"lang"], "Math") }, { cf_name = "floor" }) ) }, _)
				| TCall( { eexpr = TField( _, FStatic({ cl_path = (["java";"lang"], "Math") }, { cf_name = "round" }) ) }, _)
				| TCall( { eexpr = TField( _, FStatic({ cl_path = (["java";"lang"], "Math") }, { cf_name = "ceil" }) ) }, _) ->
						mk_cast basic.tint (Type.map_expr run { e with etype = basic.tfloat })
				| TCall( ( { eexpr = TField( _, FStatic({ cl_path = (["java";"lang"], "Math") }, { cf_name = "isFinite" }) ) } as efield ), [v]) ->
					{ e with eexpr = TCall( mk_static_field_access_infer runtime_cl "isFinite" efield.epos [], [run v] ) }
				(* end of math changes *)

				(* Std.is() *)
				| TCall(
						{ eexpr = TField( _, FStatic({ cl_path = ([], "Std") }, { cf_name = ("is" | "isOfType") })) },
						[ obj; { eexpr = TTypeExpr(md) } ]
					) ->
					let mk_is is_basic obj md =
						let obj = if is_basic then mk_cast t_dynamic obj else obj in
						{ e with eexpr = TCall( { eexpr = TIdent "__is__"; etype = t_dynamic; epos = e.epos }, [
							run obj;
							{ eexpr = TTypeExpr md; etype = t_dynamic (* this is after all a syntax filter *); epos = e.epos }
						] ) }
					in
					(match follow_module follow md with
						| TAbstractDecl({ a_path = ([], "Float") }) ->
							{
								eexpr = TCall(
									mk_static_field_access_infer runtime_cl "isDouble" e.epos [],
									[ run obj ]
								);
								etype = basic.tbool;
								epos = e.epos
							}
						| TAbstractDecl{ a_path = ([], "Int") } ->
							{
								eexpr = TCall(
									mk_static_field_access_infer runtime_cl "isInt" e.epos [],
									[ run obj ]
								);
								etype = basic.tbool;
								epos = e.epos
							}
						| TAbstractDecl{ a_path = ([], "Bool") } ->
							mk_is true obj bool_md
						| TAbstractDecl{ a_path = ([], "Single") } ->
							mk_is true obj f_md
						| TAbstractDecl{ a_path = (["java"], "Int8") } ->
							mk_is true obj i8_md
						| TAbstractDecl{ a_path = (["java"], "Int16") } ->
							mk_is true obj i16_md
						| TAbstractDecl{ a_path = (["java"], "Char16") } ->
							mk_is true obj c16_md
						| TAbstractDecl{ a_path = (["java"], "Int64") } ->
							mk_is true obj i64_md
						| TClassDecl{ cl_path = (["haxe"], "Int64") } ->
							mk_is true obj i64_md
						| TAbstractDecl{ a_path = ([], "Dynamic") }
						| TClassDecl{ cl_path = ([], "Dynamic") } when not (is_nullable obj.etype) ->
							(match obj.eexpr with
								| TLocal _ | TConst _ -> { e with eexpr = TConst(TBool true) }
								| _ -> { e with eexpr = TBlock([run obj; { e with eexpr = TConst(TBool true) }]) }
							)
						| _ ->
							if not (is_java_basic_type obj.etype) then
								try
									Type.unify obj.etype (type_of_module_type md);
									mk_is false obj md
								with Unify_error _ -> e
							else e
					)
				(* end Std.is() *)
				| _ -> Type.map_expr run e
		in
		gen.gsyntax_filters#add name (PCustom priority) run

end;;


(* ******************************************* *)
(* JavaSpecificSynf *)
(* ******************************************* *)
(*
	Some Java-specific syntax filters that can run after ExprUnwrap

	dependencies:
		Runs after ExprUnwarp
*)
module JavaSpecificSynf =
struct
	let name = "java_specific"
	let priority = solve_deps name [ DAfter ExpressionUnwrap.priority; DAfter ObjectDeclMap.priority; DAfter ArrayDeclSynf.priority; DBefore IntDivisionSynf.priority ]

	let java_hash s =
		let high_surrogate c = (c lsr 10) + 0xD7C0 in
		let low_surrogate c = (c land 0x3FF) lor 0xDC00 in
		let h = ref Int32.zero in
		let thirtyone = Int32.of_int 31 in
		(try
			UTF8.validate s;
			UTF8.iter (fun c ->
				let c = (UCharExt.code c) in
				if c > 0xFFFF then
					(h := Int32.add (Int32.mul thirtyone !h)
						(Int32.of_int (high_surrogate c));
					h := Int32.add (Int32.mul thirtyone !h)
						(Int32.of_int (low_surrogate c)))
				else
					h := Int32.add (Int32.mul thirtyone !h)
						(Int32.of_int c)
				) s
		with UTF8.Malformed_code ->
			String.iter (fun c ->
				h := Int32.add (Int32.mul thirtyone !h)
					(Int32.of_int (Char.code c))) s
		);
		!h

	let rec is_final_return_expr is_switch e =
		let is_final_return_expr = is_final_return_expr is_switch in
		match e.eexpr with
			| TReturn _
			| TThrow _ -> true
			(* this is hack to not use 'break' on switch cases *)
			| TIdent "__fallback__" when is_switch -> true
			| TMeta ((Meta.LoopLabel,_,_), { eexpr = TBreak }) -> true
			| TParenthesis p | TMeta (_,p) -> is_final_return_expr p
			| TBlock bl -> is_final_return_block is_switch bl
			| TSwitch (_, el_e_l, edef) ->
				List.for_all (fun (_,e) -> is_final_return_expr e) el_e_l && Option.map_default is_final_return_expr false edef
			| TIf (_,eif, Some eelse) ->
				is_final_return_expr eif && is_final_return_expr eelse
			| TFor (_,_,e) ->
				is_final_return_expr e
			| TWhile (_,e,_) ->
				is_final_return_expr e
			| TFunction tf ->
				is_final_return_expr tf.tf_expr
			| TTry (e, ve_l) ->
				is_final_return_expr e && List.for_all (fun (_,e) -> is_final_return_expr e) ve_l
			| _ -> false

	and is_final_return_block is_switch el =
		match el with
			| [] -> false
			| final :: [] -> is_final_return_expr is_switch final
			| hd :: tl -> is_final_return_block is_switch tl

	let rec is_null e = match e.eexpr with
		| TConst(TNull) -> true
		| TParenthesis(e)
		| TMeta(_,e) -> is_null e
		| _ -> false

	let rec is_equatable gen t =
		match follow t with
			| TInst(cl,_) ->
				if cl.cl_path = (["haxe";"lang"], "IEquatable") then
					true
				else
					List.exists (fun (cl,p) -> is_equatable gen (TInst(cl,p))) cl.cl_implements
						|| (match cl.cl_super with | Some(cl,p) -> is_equatable gen (TInst(cl,p)) | None -> false)
			| _ -> false

	(*
		Changing string switch
		will take an expression like
		switch(str)
		{
			case "a":
			case "b":
		}

		and modify it to:
		{
			var execute_def = true;
			switch(str == null ? 0 : str.hashCode())
			{
				case (hashcode of a):
					if (str == "a")
					{
						execute_def = false;
						..code here
					} //else if (str == otherVariableWithSameHashCode) {
						...
					}
				...
			}
			if (execute_def)
			{
				..default code
			}
		}

		this might actually be slower in some cases than a if/else approach, but it scales well and as a bonus,
		hashCode in java are cached, so we only have the performance hit once to cache it.
	*)
	let change_string_switch gen eswitch e1 ecases edefault =
		let basic = gen.gcon.basic in
		let is_final_ret = is_final_return_expr false eswitch in

		let has_default = is_some edefault in
		let block = ref [] in
		let local = match e1.eexpr with
			| TLocal _ -> e1
			| _ ->
				let var = mk_temp "svar" e1.etype in
				let added = { e1 with eexpr = TVar(var, Some(e1)); etype = basic.tvoid } in
				let local = mk_local var e1.epos in
				block := added :: !block;
				local
		in
		let execute_def_var = mk_temp "executeDef" gen.gcon.basic.tbool in
		let execute_def = mk_local execute_def_var e1.epos in
		let execute_def_set = { eexpr = TBinop(Ast.OpAssign, execute_def, { eexpr = TConst(TBool false); etype = basic.tbool; epos = e1.epos }); etype = basic.tbool; epos = e1.epos } in

		let hash_cache = ref None in

		let local_hashcode = ref { local with
			eexpr = TCall({ local with
				eexpr = TField(local, FDynamic "hashCode");
				etype = TFun([], basic.tint);
			}, []);
			etype = basic.tint
		} in

		let get_hash_cache () =
			match !hash_cache with
				| Some c -> c
				| None ->
					let var = mk_temp "hash" basic.tint in
					let cond = !local_hashcode in
					block := { eexpr = TVar(var, Some cond); etype = basic.tvoid; epos = local.epos } :: !block;
					let local = mk_local var local.epos in
					local_hashcode := local;
					hash_cache := Some local;
					local
		in

		let has_case = ref false in
		(* first we need to reorder all cases so all collisions are close to each other *)

		let get_str e = match e.eexpr with | TConst(TString s) -> s | _ -> die "" __LOC__ in
		let has_conflict = ref false in

		let rec reorder_cases unordered ordered =
			match unordered with
				| [] -> ordered
				| (el, e) :: tl ->
					let current = Hashtbl.create 1 in
					List.iter (fun e ->
						let str = get_str e in
						let hash = java_hash str in
						Hashtbl.add current hash true
					) el;

					let rec extract_fields cases found_cases ret_cases =
						match cases with
							| [] -> found_cases, ret_cases
							| (el, e) :: tl ->
								if List.exists (fun e -> Hashtbl.mem current (java_hash (get_str e)) ) el then begin
									has_conflict := true;
									List.iter (fun e -> Hashtbl.add current (java_hash (get_str e)) true) el;
									extract_fields tl ( (el, e) :: found_cases ) ret_cases
								end else
									extract_fields tl found_cases ( (el, e) :: ret_cases )
					in
					let found, remaining = extract_fields tl [] [] in
					let ret = if found <> [] then
						let ret = List.sort (fun (e1,_) (e2,_) -> compare (List.length e2) (List.length e1) ) ( (el, e) :: found ) in
						let rec loop ret acc =
							match ret with
								| (el, e) :: ( (_,_) :: _ as tl ) -> loop tl ( (true, el, e) :: acc )
								| (el, e) :: [] -> ( (false, el, e) :: acc )
								| _ -> die "" __LOC__
						in
						List.rev (loop ret [])
					else
						(false, el, e) :: []
					in

					reorder_cases remaining (ordered @ ret)
		in

		let already_in_cases = Hashtbl.create 0 in
		let change_case (has_fallback, el, e) =
			let conds, el = List.fold_left (fun (conds,el) e ->
				has_case := true;
				match e.eexpr with
					| TConst(TString s) ->
						let hashed = java_hash s in
						let equals_test = {
							eexpr = TCall({ e with eexpr = TField(local, FDynamic "equals"); etype = TFun(["obj",false,t_dynamic],basic.tbool) }, [ e ]);
							etype = basic.tbool;
							epos = e.epos
						} in

						let hashed_expr = { eexpr = TConst(TInt hashed); etype = basic.tint; epos = e.epos } in
						let hashed_exprs = if !has_conflict then begin
							if Hashtbl.mem already_in_cases hashed then
								el
							else begin
								Hashtbl.add already_in_cases hashed true;
								hashed_expr :: el
							end
						end else hashed_expr :: el in

						let conds = match conds with
							| None -> equals_test
							| Some c ->
								(*
									if there is more than one case, we should test first if hash equals to the one specified.
									This way we can save a heavier string compare
								*)
								let equals_test = mk_paren {
									eexpr = TBinop(Ast.OpBoolAnd, { eexpr = TBinop(Ast.OpEq, get_hash_cache(), hashed_expr); etype = basic.tbool; epos = e.epos }, equals_test);
									etype = basic.tbool;
									epos = e.epos;
								} in

								{ eexpr = TBinop(Ast.OpBoolOr, equals_test, c); etype = basic.tbool; epos = e1.epos }
						in

						Some conds, hashed_exprs
					| _ -> die "" __LOC__
			) (None,[]) el in
			let e = if has_default then Type.concat execute_def_set e else e in
			let e = if !has_conflict then Type.concat e { e with eexpr = TBreak; etype = basic.tvoid } else e in
			let e = {
				eexpr = TIf(get conds, e, None);
				etype = basic.tvoid;
				epos = e.epos
			} in

			let e = if has_fallback then { e with eexpr = TBlock([ e; mk (TIdent "__fallback__") t_dynamic e.epos]) } else e in

			(el, e)
		in

		let is_not_null_check = mk (TBinop (OpNotEq, local, { local with eexpr = TConst TNull })) basic.tbool local.epos in
		let if_not_null e = { e with eexpr = TIf (is_not_null_check, e, None) } in
		let switch = if_not_null { eswitch with
			eexpr = TSwitch(!local_hashcode, List.map change_case (reorder_cases ecases []), None);
		} in
		(if !has_case then begin
			(if has_default then block := { e1 with eexpr = TVar(execute_def_var, Some({ e1 with eexpr = TConst(TBool true); etype = basic.tbool })); etype = basic.tvoid } :: !block);
			block := switch :: !block
		end);
		(match edefault with
			| None -> ()
			| Some edef when not !has_case ->
				block := edef :: !block
			| Some edef ->
				let eelse = if is_final_ret then Some { eexpr = TThrow { eexpr = TConst(TNull); etype = t_dynamic; epos = edef.epos }; etype = basic.tvoid; epos = edef.epos } else None in
				block := { edef with eexpr = TIf(execute_def, edef, eelse); etype = basic.tvoid } :: !block
		);
		{ eswitch with eexpr = TBlock(List.rev !block) }


	let get_cl_from_t t =
		match follow t with
		| TInst(cl,_) -> cl
		| _ -> die "" __LOC__

	let configure gen runtime_cl =
		let cl_boolean = get_cl (get_type gen (["java";"lang"],"Boolean")) in
		let cl_number = get_cl (get_type gen (["java";"lang"],"Number")) in

		(if java_hash "Testing string hashCode implementation from haXe" <> (Int32.of_int 545883604) then die "" __LOC__);
		let basic = gen.gcon.basic in
		let tbyte = mt_to_t_dyn ( get_type gen (["java"], "Int8") ) in
		let tshort = mt_to_t_dyn ( get_type gen (["java"], "Int16") ) in
		let tsingle = mt_to_t_dyn ( get_type gen ([], "Single") ) in
		let ti64 = mt_to_t_dyn ( get_type gen (["java"], "Int64") ) in
		let string_ext = get_cl ( get_type gen (["haxe";"lang"], "StringExt")) in
		let fast_cast = Common.defined gen.gcon Define.FastCast in

		let get_unboxed_from_boxed boxed_t =
			match boxed_t with
				| TInst( ({ cl_path = (["java";"lang"],name) } as cl), [] ) -> (match name with
					| "Double" ->
						cl, basic.tfloat
					| "Integer" ->
						cl, basic.tint
					| "Byte" ->
						cl, tbyte
					| "Short" ->
						cl, tshort
					| "Float" ->
						cl, tsingle
					| "Long" ->
						cl, ti64
					| _ ->
						die "" __LOC__)
				| _ -> die "" __LOC__
		in

		let mk_valueof_call boxed_t expr =
			let box_cl, unboxed_t = get_unboxed_from_boxed boxed_t in
			let fn = TFun(["param1",false,unboxed_t],boxed_t) in
			{
				eexpr = TCall(mk_static_field_access box_cl "valueOf" fn expr.epos, [mk_cast_if_needed unboxed_t expr]);
				etype = boxed_t;
				epos = expr.epos;
			}
		in

		let mk_unbox unboxed_t boxed_e =
			if is_int64_type unboxed_t then
				{
					eexpr = TCall(
						mk_static_field_access_infer runtime_cl "getInt64FromNumber" boxed_e.epos [],
						[ boxed_e ]
					);
					etype = ti64;
					epos = boxed_e.epos
				}
			else if is_int_type unboxed_t then
				mk_cast_if_needed unboxed_t {
					eexpr = TCall(
						mk_static_field_access_infer runtime_cl "getIntFromNumber" boxed_e.epos [],
						[ boxed_e ]
					);
					etype = basic.tint;
					epos = boxed_e.epos
				}
			else
				mk_cast_if_needed unboxed_t {
					eexpr = TCall(
						mk_static_field_access_infer runtime_cl "getFloatFromNumber" boxed_e.epos [],
						[ boxed_e ]
					);
					etype = basic.tfloat;
					epos = boxed_e.epos
				}
		in

		let mk_dyn_box boxed_t expr =
			let name = match boxed_t with
				| TInst({ cl_path = (["java";"lang"],"Integer") },[]) ->
					"numToInteger"
				| TInst({ cl_path = (["java";"lang"],"Double") },[]) ->
					"numToDouble"
				| TInst({ cl_path = (["java";"lang"],"Float") },[]) ->
					"numToFloat"
				| TInst({ cl_path = (["java";"lang"],"Byte") },[]) ->
					"numToByte"
				| TInst({ cl_path = (["java";"lang"],"Long") },[]) ->
					"numToLong"
				| TInst({ cl_path = (["java";"lang"],"Short") },[]) ->
					"numToShort"
				| _ -> gen.gcon.error ("Invalid boxed type " ^ (debug_type boxed_t)) expr.epos; die "" __LOC__
			in
			{
				eexpr = TCall(
					mk_static_field_access_infer runtime_cl name expr.epos [],
					[ mk_cast (TInst(cl_number,[])) expr ]
				);
				etype = boxed_t;
				epos = expr.epos
			}
		in

		let rec run e =
			match e.eexpr with
				(* for new NativeArray<T> issues *)
				| TNew(({ cl_path = (["java"], "NativeArray") } as cl), [t], el) when is_type_param t ->
					mk_cast (TInst(cl,[t])) (mk_cast t_dynamic ({ e with eexpr = TNew(cl, [t_empty], List.map run el) }))

				(* Std.int() *)
				| TCall(
						{ eexpr = TField( _, FStatic({ cl_path = ([], "Std") }, { cf_name = "int" })) },
						[obj]
					) ->
					run (mk_cast basic.tint obj)
				(* end Std.int() *)

				| TField( ef, FInstance({ cl_path = ([], "String") }, _, { cf_name = "length" }) ) ->
					{ e with eexpr = TCall(Type.map_expr run e, []) }
				| TField( ef, field ) when field_name field = "length" && is_string ef.etype ->
					{ e with eexpr = TCall(Type.map_expr run e, []) }
				| TCall( ( { eexpr = TField(ef, field) } as efield ), args ) when is_string ef.etype && String.get (field_name field) 0 = '_' ->
					let field = field_name field in
					{ e with eexpr = TCall({ efield with eexpr = TField(run ef, FDynamic (String.sub field 1 ( (String.length field) - 1)) )}, List.map run args) }
				| TCall( ( { eexpr = TField(ef, FInstance({ cl_path = [], "String" }, _, field )) } as efield ), args ) ->
					let field = field.cf_name in
					(match field with
						| "charAt" | "charCodeAt" | "split" | "indexOf"
						| "lastIndexOf" | "substring" | "substr" ->
							{ e with eexpr = TCall(mk_static_field_access_infer string_ext field e.epos [], [run ef] @ (List.map run args)) }
						| _ ->
							{ e with eexpr = TCall(run efield, List.map run args) }
					)

				| TCast(expr, _) when is_boxed_number (gen.greal_type expr.etype) && is_unboxed_number (gen.greal_type e.etype) ->
					let to_t = gen.greal_type e.etype in
					mk_unbox to_t (run expr)

				| TCast(expr, md) when is_boxed_number (gen.greal_type e.etype) ->
					let to_t = gen.greal_type e.etype in
					let from_t = gen.greal_type expr.etype in
					let ret = if is_unboxed_number from_t then
							mk_valueof_call to_t (run expr)
						else if is_boxed_number from_t then
							if type_iseq from_t to_t then begin
								(* special case for when the expression is null, as we sometimes need to give *)
								(* a little help to Java's typer *)
								if is_null expr then
									{ e with eexpr = TCast(run expr, md) }
								else
									run expr
							end else
								mk_dyn_box (gen.greal_type e.etype) (run expr)
						else begin
							if in_runtime_class gen then
								mk_cast e.etype (run expr)
							else
								mk_dyn_box (gen.greal_type e.etype) (run expr)
						end
					in
					ret

				| TCast(expr, _) when is_bool e.etype ->
					{
						eexpr = TCall(
							mk_static_field_access_infer runtime_cl "toBool" expr.epos [],
							[ mk_cast_if_needed (TInst(cl_boolean,[])) (run expr) ]
						);
						etype = basic.tbool;
						epos = e.epos
					}

				| TCast(expr, _) when is_int_float gen e.etype && is_dynamic gen expr.etype ->
					let needs_cast = match gen.gfollow#run_f e.etype with
						| TInst _ -> false
						| _ -> true
					in

					let fun_name = if like_int e.etype then "toInt" else "toDouble" in

					let ret = {
						eexpr = TCall(
							mk_static_field_access_infer runtime_cl fun_name expr.epos [],
							[ run expr ]
						);
						etype = if fun_name = "toDouble" then basic.tfloat else basic.tint;
						epos = expr.epos
					} in

					if needs_cast then mk_cast e.etype ret else ret

				| TCast(expr, _) when like_i64 e.etype && is_dynamic gen expr.etype ->
					{
						eexpr = TCall(
							mk_static_field_access_infer runtime_cl "toLong" expr.epos [],
							[ run expr ]
						);
						etype = ti64;
						epos = expr.epos
					}

				| TCast(expr, Some(TClassDecl cls)) when fast_cast && cls == null_class ->
					{ e with eexpr = TCast(run expr, Some(TClassDecl null_class)) }

				| TBinop( (Ast.OpAssignOp OpAdd as op), e1, e2)
				| TBinop( (Ast.OpAdd as op), e1, e2) when not fast_cast && (is_string e.etype || is_string e1.etype || is_string e2.etype) ->
						let is_assign = match op with Ast.OpAssignOp _ -> true | _ -> false in
						let mk_to_string e = { e with eexpr = TCall( mk_static_field_access_infer runtime_cl "toString" e.epos [], [run e] ); etype = gen.gcon.basic.tstring	} in
						let check_cast e = match gen.greal_type e.etype with
							| TDynamic _
							| TAbstract({ a_path = ([], "Float") }, [])
							| TAbstract({ a_path = ([], "Single") }, []) ->
									mk_to_string e
							| _ -> run e
						in

						{ e with eexpr = TBinop(op, (if is_assign then run e1 else check_cast e1), check_cast e2) }
				| TCast(expr, _) when is_string e.etype ->
					{ e with eexpr = TCall( mk_static_field_access_infer runtime_cl "toString" expr.epos [], [run expr] ) }

				| TSwitch(cond, ecases, edefault) when is_string cond.etype ->
					(*let change_string_switch gen eswitch e1 ecases edefault =*)
					change_string_switch gen e (run cond) (List.map (fun (el,e) -> (el, run e)) ecases) (Option.map run edefault)

				| TBinop( (Ast.OpNotEq as op), e1, e2)
				| TBinop( (Ast.OpEq as op), e1, e2) when not (is_null e2 || is_null e1) && (is_string e1.etype || is_string e2.etype || is_equatable gen e1.etype || is_equatable gen e2.etype) ->
					let static = mk_static_field_access_infer (runtime_cl) "valEq" e1.epos [] in
					let eret = { eexpr = TCall(static, [run e1; run e2]); etype = gen.gcon.basic.tbool; epos=e.epos } in
					if op = Ast.OpNotEq then { eret with eexpr = TUnop(Ast.Not, Ast.Prefix, eret) } else eret

				| TBinop( (Ast.OpNotEq | Ast.OpEq as op), e1, e2) when is_cl e1.etype && is_cl e2.etype ->
					{ e with eexpr = TBinop(op, mk_cast t_empty (run e1), mk_cast t_empty (run e2)) }
				| _ -> Type.map_expr run e
		in
		gen.gsyntax_filters#add name (PCustom priority) run
end;;


(* ******************************************* *)
(* handle @:throws *)
(* ******************************************* *)
let rec is_checked_exc cl =
	match cl.cl_path with
		| ["java";"lang"],"RuntimeException" ->
			false
		| ["java";"lang"],"Throwable" ->
			true
		| _ -> match cl.cl_super with
			| None -> false
			| Some(c,_) -> is_checked_exc c

let rec cls_any_super cl supers =
	PMap.mem cl.cl_path supers || match cl.cl_super with
		| None -> false
		| Some(c,_) -> cls_any_super c supers

let rec handle_throws gen cf =
	List.iter (handle_throws gen) cf.cf_overloads;
	match cf.cf_expr with
	| Some ({ eexpr = TFunction(tf) } as e)  ->
		let rec collect_throws acc = function
			| (Meta.Throws, [Ast.EConst (Ast.String(path,_)), _],_) :: meta -> (try
				collect_throws (get_cl ( get_type gen (parse_path path)) :: acc) meta
			with | Not_found | TypeNotFound _ ->
				collect_throws acc meta)
			| [] ->
				acc
			| _ :: meta ->
				collect_throws acc meta
		in
		let cf_throws = collect_throws [] cf.cf_meta in
		let throws = ref (List.fold_left (fun map cl ->
			PMap.add cl.cl_path cl map
		) PMap.empty cf_throws) in
		let rec iter e = match e.eexpr with
			| TTry(etry,ecatches) ->
				let old = !throws in
				let needs_check_block = ref true in
				List.iter (fun (v,e) ->
					Type.iter iter e;
					match follow (run_follow gen v.v_type) with
						| TInst({ cl_path = ["java";"lang"],"Throwable" },_)
						| TDynamic _ ->
							needs_check_block := false
						| TInst(c,_) when is_checked_exc c ->
							throws := PMap.add c.cl_path c !throws
						| _ ->()
				) ecatches;
				if !needs_check_block then Type.iter iter etry;
				throws := old
			| TField(e, (FInstance(_,_,f) | FStatic(_,f) | FClosure(_,f))) ->
				let tdefs = collect_throws [] f.cf_meta in
				if tdefs <> [] && not (List.for_all (fun c -> cls_any_super c !throws) tdefs) then
					raise Exit;
				Type.iter iter e
			| TThrow e -> (match follow (run_follow gen e.etype) with
				| TInst(c,_) when is_checked_exc c && not (cls_any_super c !throws) ->
					raise Exit
				| _ -> iter e)
			| _ -> Type.iter iter e
		in
		(try
			Type.iter iter e
		with | Exit -> (* needs typed exception to be caught *)
			let throwable = get_cl (get_type gen (["java";"lang"],"Throwable")) in
			let cast_cl = get_cl (get_type gen (["java";"lang"],"RuntimeException")) in
			let catch_var = alloc_var "typedException" (TInst(throwable,[])) in
			let rethrow = mk_local catch_var e.epos in
			let hx_exception = get_cl (get_type gen (["haxe"], "Exception")) in
			let wrap_static = mk_static_field_access (hx_exception) "thrown" (TFun([("obj",false,t_dynamic)], t_dynamic)) rethrow.epos in
			let thrown_value = mk_cast (TInst(cast_cl,[])) { rethrow with eexpr = TCall(wrap_static, [rethrow]) } in
			let wrapped = { rethrow with eexpr = TThrow thrown_value; } in
			let map_throws cl =
				let var = alloc_var "typedException" (TInst(cl,List.map (fun _ -> t_dynamic) cl.cl_params)) in
				var, { tf.tf_expr with eexpr = TThrow (mk_local var e.epos) }
			in
			cf.cf_expr <- Some { e with
				eexpr = TFunction({ tf with
					tf_expr = mk_block { tf.tf_expr with eexpr = TTry(tf.tf_expr, List.map (map_throws) cf_throws @ [catch_var, wrapped]) }
				})
			})
	| _ -> ()