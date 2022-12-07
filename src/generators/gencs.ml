
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
open ReflectionCFs
open Globals
open Ast
open Common
open Type
open Gencommon
open Gencommon.SourceWriter
open Codegen
open Texpr.Builder
open Printf
open Option
open ExtString

type cs_native_constraint =
	| CsStruct
	| CsClass
	| CsUnmanaged
	| CsConstructible
	| CsConstraint of string

let get_constraint = function
	| CsStruct -> "struct"
	| CsClass -> "class"
	| CsUnmanaged -> "unmanaged"
	| CsConstructible -> "new()"
	| CsConstraint s -> s

let rec is_cs_basic_type t =
	match follow t with
		| TInst( { cl_path = (["haxe"], "Int32") }, [] )
		| TInst( { cl_path = (["haxe"], "Int64") }, [] )
		| TAbstract ({ a_path = (["cs"], "Int64") },[])
		| TAbstract ({ a_path = (["cs"], "UInt64") },[])
		| TAbstract ({ a_path = ([], "Int") },[])
		| TAbstract ({ a_path = ([], "Float") },[])
		| TAbstract ({ a_path = ([], "Bool") },[]) ->
			true
		| TAbstract ({ a_path = (["cs"], "Pointer") },_) ->
			false
		| TAbstract _ when like_float t ->
			true
		| TAbstract(a,pl) when not (Meta.has Meta.CoreType a.a_meta) ->
			is_cs_basic_type (Abstract.get_underlying_type a pl)
		| TEnum(e, _) as t when not (is_hxgen_t t) -> true
		| TInst(cl, _) when Meta.has Meta.Struct cl.cl_meta -> true
		| _ -> false

let binops_names = List.fold_left (fun acc (op,n) -> PMap.add n op acc) PMap.empty Dotnet.cs_binops
let unops_names = List.fold_left (fun acc (op,n) -> PMap.add n op acc) PMap.empty Dotnet.cs_unops

let is_tparam t =
	match follow t with
		| TInst( { cl_kind = KTypeParameter _ }, [] ) -> true
		| _ -> false

let rec is_int_float gen t =
	match follow (gen.greal_type t) with
		| TInst( { cl_path = (["haxe"], "Int32") }, [] )
		| TAbstract ({ a_path = ([], "Int") },[])
		| TAbstract ({ a_path = ([], "Float") },[]) ->
			true
		| TAbstract _ when like_float t && not (like_i64 t) ->
			true
		| TInst( { cl_path = (["haxe"; "lang"], "Null") }, [t] ) -> is_int_float gen t
		| _ -> false

let is_bool t =
	match follow t with
		| TAbstract ({ a_path = ([], "Bool") },[]) ->
			true
		| _ -> false

let is_exactly_bool gen t =
	match gen.gfollow#run_f t with
		| TAbstract ({ a_path = ([], "Bool") },[]) ->
			true
		| _ -> false

let is_dynamic gen t =
	match follow (gen.greal_type t) with
		| TDynamic _ -> true
		| _ -> false

let is_pointer gen t =
	match follow (gen.greal_type t) with
		| TAbstract( ( {a_path = ["cs"], "Pointer"}, _ ) )
		| TInst( {cl_path = ["cs"], "Pointer"}, _ ) -> true
		| _ -> false

let rec is_null t =
	match t with
		| TInst( { cl_path = (["haxe"; "lang"], "Null") }, _ )
		| TAbstract( { a_path = ([], "Null") }, _ ) -> true
		| TType( t, tl ) -> is_null (apply_typedef t tl)
		| TMono r ->
			(match r.tm_type with
			| Some t -> is_null t
			| _ -> false)
		| TLazy f ->
			is_null (lazy_type f)
		| _ -> false

let rec get_ptr e = match e.eexpr with
	| TParenthesis e | TMeta(_,e)
	| TCast(e,_) -> get_ptr e
	| TCall( { eexpr = TIdent "__ptr__" }, [ e ] ) ->
		Some e
	| _ -> None

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

let rec change_md = function
	| TAbstractDecl(a) when Meta.has Meta.Delegate a.a_meta && not (Meta.has Meta.CoreType a.a_meta) ->
		change_md (t_to_md a.a_this)
	| TClassDecl( { cl_kind = KAbstractImpl ({ a_this = TInst(impl,_) } as a) }) when Meta.has Meta.Delegate a.a_meta ->
		TClassDecl impl
	| TClassDecl( { cl_kind = KAbstractImpl (a) }) when Meta.has Meta.CoreType a.a_meta ->
		TAbstractDecl a
	| md -> md

(**
	Generates method overloads for a method with trailing optional arguments.
	E.g. for `function method(a:Int, b:Bool = false) {...}`
	generates `function method(a:Int) { method(a, false); }`
*)
let get_overloads_for_optional_args gen cl cf is_static =
	match cf.cf_params,cf.cf_kind with
	| [],Method (MethNormal | MethDynamic | MethInline) ->
		(match cf.cf_expr, follow cf.cf_type with
		| Some ({ eexpr = TFunction fn } as method_expr), TFun (args, return_type) ->
			let type_params = extract_param_types cl.cl_params in
			let rec collect_overloads tf_args_rev args_rev default_values_rev =
				match tf_args_rev, args_rev with
				| (_, Some default_value) :: rest_tf_args_rev, _ :: rest_args_rev ->
					let field_expr =
						let cl_type = TInst (cl,type_params) in
						if cf.cf_name = "new" then
							mk (TConst TThis) cl_type cf.cf_pos
						else if is_static then
							let class_expr =
								mk (TTypeExpr (TClassDecl cl)) cl_type cf.cf_pos
							in
							mk (TField (class_expr, FStatic(cl,cf))) cf.cf_type cf.cf_pos
						else
							let this_expr =
								mk (TConst TThis) cl_type cf.cf_pos
							in
							mk (TField (this_expr, FInstance(cl,type_params,cf))) cf.cf_type cf.cf_pos
					in
					let default_values_rev = default_values_rev @ [default_value] in
					let args_exprs =
						List.rev (
							default_values_rev
							@ (List.map (fun (v,_) -> mk_local v v.v_pos ) rest_tf_args_rev)
						)
					in
					let call = { fn.tf_expr with eexpr = TCall (field_expr, args_exprs) } in
					let fn_body =
						if ExtType.is_void (follow return_type) then call
						else { fn.tf_expr with eexpr = TReturn (Some call) }
					in
					let fn =
						{ fn with tf_args = List.rev rest_tf_args_rev; tf_expr = mk_block fn_body }
					in
					{ cf with
						cf_overloads = [];
						cf_type = TFun (List.rev rest_args_rev, return_type);
						cf_expr = Some { method_expr with eexpr = TFunction fn };
					} :: collect_overloads rest_tf_args_rev rest_args_rev default_values_rev
				| _ -> []
			in
			collect_overloads (List.rev fn.tf_args) (List.rev args) []
		| _ -> []
		)
	| _ -> []

(* used in c#-specific filters to skip some of them for the special haxe.lang.Runtime class *)
let in_runtime_class gen =
	match gen.gcurrent_class with
	| Some { cl_path = ["haxe";"lang"],"Runtime"} -> true
	| _ -> false

(* ******************************************* *)
(* CSharpSpecificESynf *)
(* ******************************************* *)
(*
	Some CSharp-specific syntax filters that must run before ExpressionUnwrap

	dependencies:
		It must run before ExprUnwrap, as it may not return valid Expr/Statement expressions
		It must run before ClassInstance, as it will detect expressions that need unchanged TTypeExpr
*)
module CSharpSpecificESynf =
struct
	let name = "csharp_specific_e"
	let priority = solve_deps name [DBefore ExpressionUnwrap.priority; DBefore ClassInstance.priority]

	let get_cl_from_t t =
		match follow t with
			| TInst(cl,_) -> cl
			| _ -> die "" __LOC__

	let get_ab_from_t t =
		match follow t with
			| TAbstract(ab,_) -> ab
			| _ -> die "" __LOC__

	let configure gen runtime_cl =
		let basic = gen.gcon.basic in
		let uint = match get_type gen ([], "UInt") with | TTypeDecl t -> TType(t, []) | TAbstractDecl a -> TAbstract(a, []) | _ -> die "" __LOC__ in

		let rec run e =
			match e.eexpr with
				(* Std.is() *)
				| TCall(
						{ eexpr = TField( _, FStatic({ cl_path = ([], "Std") }, { cf_name = ("is" | "isOfType") })) },
						[ obj; { eexpr = TTypeExpr(TClassDecl { cl_path = [], "Dynamic" } | TAbstractDecl { a_path = [], "Dynamic" }) }]
					) ->
						Type.map_expr run e
				| TCall(
						{ eexpr = TField( _, FStatic({ cl_path = ([], "Std") }, { cf_name = ("is" | "isOfType") }) ) },
						[ obj; { eexpr = TTypeExpr(md) }]
					) ->
					let md = change_md md in
					let mk_is obj md =
						{ e with eexpr = TCall( { eexpr = TIdent "__is__"; etype = t_dynamic; epos = e.epos }, [
							obj;
							{ eexpr = TTypeExpr md; etype = t_dynamic (* this is after all a syntax filter *); epos = e.epos }
						] ) }
					in

					let mk_or a b =
						{
							eexpr = TBinop(Ast.OpBoolOr, a, b);
							etype = basic.tbool;
							epos = e.epos
						}
					in

					let wrap_if_needed obj f =
						(* introduce temp variable for complex expressions *)
						match obj.eexpr with
							| TLocal(v) -> f obj
							| _ ->
								let var = mk_temp "isOfType" obj.etype in
								let added = { obj with eexpr = TVar(var, Some(obj)); etype = basic.tvoid } in
								let local = mk_local var obj.epos in
								{
									eexpr = TBlock([ added; f local ]);
									etype = basic.tbool;
									epos = e.epos
								}
					in

					let obj = run obj in
					(match follow_module follow md with
						| TAbstractDecl{ a_path = ([], "Float") } when not (in_runtime_class gen) ->
							(* on the special case of seeing if it is a Float, we need to test if both it is a float and if it is an Int *)
							let mk_is local =
								(* we check if it float or int or uint *)
								let eisint = mk_is local (TAbstractDecl (get_ab_from_t basic.tint)) in
								let eisuint = mk_is local (TAbstractDecl (get_ab_from_t uint)) in
								let eisfloat = mk_is local md in
								mk_paren (mk_or eisfloat (mk_or eisint eisuint))
							in
							wrap_if_needed obj mk_is

						| TAbstractDecl{ a_path = ([], "Int") } when not (in_runtime_class gen) ->
							(* int can be stored in double variable because of anonymous functions, check that case *)
							let mk_isint_call local =
								{
									eexpr = TCall(
										mk_static_field_access_infer runtime_cl "isInt" e.epos [],
										[ local ]
									);
									etype = basic.tbool;
									epos = e.epos
								}
							in
							let mk_is local =
								let eisint = mk_is local (TAbstractDecl (get_ab_from_t basic.tint)) in
								let eisuint = mk_is local (TAbstractDecl (get_ab_from_t uint)) in
								mk_paren (mk_or (mk_or eisint eisuint) (mk_isint_call local))
							in
							wrap_if_needed obj mk_is

						| TAbstractDecl{ a_path = ([], "UInt") } when not (in_runtime_class gen) ->
							(* uint can be stored in double variable because of anonymous functions, check that case *)
							let mk_isuint_call local =
								{
									eexpr = TCall(
										mk_static_field_access_infer runtime_cl "isUInt" e.epos [],
										[ local ]
									);
									etype = basic.tbool;
									epos = e.epos
								}
							in
							let mk_is local =
								let eisuint = mk_is local (TAbstractDecl (get_ab_from_t uint)) in
								mk_paren (mk_or eisuint (mk_isuint_call local))
							in
							wrap_if_needed obj mk_is

						| _ ->
							mk_is obj md
					)
				(* end Std.is() *)

				| TBinop( Ast.OpUShr, e1, e2 ) ->
					mk_cast e.etype { e with eexpr = TBinop( Ast.OpShr, mk_cast uint (run e1), run e2 ) }

				| TBinop( Ast.OpAssignOp Ast.OpUShr, e1, e2 ) ->
					let mk_ushr local =
						{ e with eexpr = TBinop(Ast.OpAssign, local, run { e with eexpr = TBinop(Ast.OpUShr, local, run e2) }) }
					in

					let mk_local obj =
						let var = mk_temp "opUshr" obj.etype in
						let added = { obj with eexpr = TVar(var, Some(obj)); etype = basic.tvoid } in
						let local = mk_local var obj.epos in
						local, added
					in

					let e1 = run e1 in

					let ret = match e1.eexpr with
						| TField({ eexpr = TLocal _ }, _)
						| TField({ eexpr = TTypeExpr _ }, _)
						| TArray({ eexpr = TLocal _ }, _)
						| TLocal(_) ->
							mk_ushr e1
						| TField(fexpr, field) ->
							let local, added = mk_local fexpr in
							{ e with eexpr = TBlock([ added; mk_ushr { e1 with eexpr = TField(local, field) }  ]); }
						| TArray(ea1, ea2) ->
							let local, added = mk_local ea1 in
							{ e with eexpr = TBlock([ added; mk_ushr { e1 with eexpr = TArray(local, ea2) }  ]); }
						| _ -> (* invalid left-side expression *)
							die "" __LOC__
					in

					ret

				| _ -> Type.map_expr run e
		in
		gen.gsyntax_filters#add name (PCustom priority) run
end;;

(* ******************************************* *)
(* CSharpSpecificSynf *)
(* ******************************************* *)

(*

	Some CSharp-specific syntax filters  that can run after ExprUnwrap

	dependencies:
		Runs after ExprUnwrap

*)

module CSharpSpecificSynf =
struct
	let name = "csharp_specific"
	let priority = solve_deps name [ DAfter ExpressionUnwrap.priority; DAfter ObjectDeclMap.priority; DAfter ArrayDeclSynf.priority;	DAfter HardNullableSynf.priority ]

	let get_cl_from_t t =
		match follow t with
			| TInst(cl,_) -> cl
			| _ -> die "" __LOC__

	let is_tparam t =
		match follow t with
			| TInst( { cl_kind = KTypeParameter _ }, _ ) -> true
			| _ -> false

	let configure gen runtime_cl =
		let basic = gen.gcon.basic in
		(* let tchar = match ( get_type gen (["cs"], "Char16") ) with
			| TTypeDecl t -> TType(t,[])
			| TAbstractDecl a -> TAbstract(a,[])
			| _ -> die "" __LOC__
		in *)
		let string_ext = get_cl ( get_type gen (["haxe";"lang"], "StringExt")) in
		let ti64 = match ( get_type gen (["cs"], "Int64") ) with | TTypeDecl t -> TType(t,[]) | TAbstractDecl a -> TAbstract(a,[]) | _ -> die "" __LOC__ in
		let boxed_ptr =
			if Common.defined gen.gcon Define.Unsafe then
				get_cl (get_type gen (["haxe";"lang"], "BoxedPointer"))
				(* get_abstract (get_type gen (["cs"],"Pointer")) *)
			else
				null_class
		in

		let is_struct t = (* not basic type *)
			match follow t with
				| TInst(cl, _) when Meta.has Meta.Struct cl.cl_meta -> true
				| _ -> false
		in

		let is_cl t = match gen.greal_type t with | TInst ( { cl_path = (["System"], "Type") }, [] ) -> true | _ -> false in

		let fast_cast = Common.defined gen.gcon Define.FastCast in

		let rec run e =
			match e.eexpr with

				(* Std.int() *)
				| TCall(
						{ eexpr = TField( _, FStatic({ cl_path = ([], "Std") }, { cf_name = "int" }) ) },
						[obj]
					) ->
					run (mk_cast basic.tint obj)
				(* end Std.int() *)

				(* TODO: change cf_name *)
				| TField(ef, FInstance({ cl_path = [], "String" }, _, { cf_name = "length" })) ->
					{ e with eexpr = TField(run ef, FDynamic "Length") }
				| TField(ef, FInstance({ cl_path = [], "String" }, _, { cf_name = "toLowerCase" })) ->
					{ e with eexpr = TField(run ef, FDynamic "ToLowerInvariant") }
				| TField(ef, FInstance({ cl_path = [], "String" }, _, { cf_name = "toUpperCase" })) ->
					{ e with eexpr = TField(run ef, FDynamic "ToUpperInvariant") }

				| TCall( { eexpr = TField(_, FStatic({ cl_path = [], "String" }, { cf_name = "fromCharCode" })) }, [cc] ) ->
					{ e with eexpr = TCall(mk_static_field_access_infer string_ext "fromCharCode" e.epos [], [run cc]) }
				| TCall( { eexpr = TField(ef, FInstance({ cl_path = [], "String" }, _, { cf_name = ("charAt" as field) })) }, args )
				| TCall( { eexpr = TField(ef, FInstance({ cl_path = [], "String" }, _, { cf_name = ("charCodeAt" as field) })) }, args )
				| TCall( { eexpr = TField(ef, FInstance({ cl_path = [], "String" }, _, { cf_name = ("indexOf" as field) })) }, args )
				| TCall( { eexpr = TField(ef, FInstance({ cl_path = [], "String" }, _, { cf_name = ("lastIndexOf" as field) })) }, args )
				| TCall( { eexpr = TField(ef, FInstance({ cl_path = [], "String" }, _, { cf_name = ("split" as field) })) }, args )
				| TCall( { eexpr = TField(ef, FInstance({ cl_path = [], "String" }, _, { cf_name = ("substring" as field) })) }, args )
				| TCall( { eexpr = TField(ef, FInstance({ cl_path = [], "String" }, _, { cf_name = ("substr" as field) })) }, args ) ->
					{ e with eexpr = TCall(mk_static_field_access_infer string_ext field e.epos [], [run ef] @ (List.map run args)) }
				| TCall( { eexpr = TField(ef, FInstance({ cl_path = [], "String" }, _, { cf_name = ("toString") })) }, [] ) ->
					run ef
				| TNew( { cl_path = ([], "String") }, [], [p] ) -> run p (* new String(myString) -> myString *)

				| TCast(expr, _) when like_float expr.etype && is_pointer gen e.etype ->
					let expr = run expr in
					mk_cast e.etype (mk_cast ti64 expr)
				| TCast(expr, _) when is_dynamic gen expr.etype && is_pointer gen e.etype ->
					(match get_ptr expr with
						| None ->
							(* unboxing *)
							let expr = run expr in
							mk_cast e.etype (mk_field_access gen (mk_cast (TInst(boxed_ptr,[])) expr) "value" e.epos)
						| Some e ->
							run e)
				| TCast(expr, _) when is_pointer gen expr.etype && is_dynamic gen e.etype ->
					(match get_ptr expr with
						| None ->
							(* boxing *)
							let expr = run expr in
							{ e with eexpr = TNew(boxed_ptr,[],[expr]) }
						| Some e ->
							run e)
				| TCast(expr, _) when is_bool e.etype && is_dynamic gen expr.etype ->
					{
						eexpr = TCall(
							mk_static_field_access_infer runtime_cl "toBool" expr.epos [],
							[ run expr ]
						);
						etype = basic.tbool;
						epos = e.epos
					}
				| TCast(expr, _) when is_int_float gen e.etype && is_dynamic gen expr.etype && ( Common.defined gen.gcon Define.EraseGenerics || not (is_null e.etype) ) && not (in_runtime_class gen) ->
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
						etype = basic.tint;
						epos = expr.epos
					} in

					if needs_cast then mk_cast e.etype ret else ret

				| TCast(expr, _) when Common.defined gen.gcon Define.EraseGenerics && like_i64 e.etype && is_dynamic gen expr.etype && not (in_runtime_class gen) ->
					{
						eexpr = TCall(
							mk_static_field_access_infer runtime_cl "toLong" expr.epos [],
							[ run expr ]
						);
						etype = ti64;
						epos = expr.epos
					}

				| TCast(expr, Some(TClassDecl cls)) when fast_cast && cls == null_class ->
					if is_cs_basic_type (gen.greal_type e.etype) || is_tparam (gen.greal_type e.etype) then
						{ e with eexpr = TCast(run expr, Some(TClassDecl null_class)) }
					else
						{ e with eexpr = TCall(mk (TIdent "__as__") t_dynamic e.epos, [run expr]) }

				| TCast(expr, _) when (is_string e.etype) && (not (is_string expr.etype)) && not (in_runtime_class gen) ->
					{ e with eexpr = TCall( mk_static_field_access_infer runtime_cl "toString" expr.epos [], [run expr] ) }

				| TCast(expr, _) when is_tparam e.etype && not (in_runtime_class gen) && not (Common.defined gen.gcon Define.EraseGenerics) ->
					let static = mk_static_field_access_infer (runtime_cl) "genericCast" e.epos [e.etype] in
					{ e with eexpr = TCall(static, [mk (TIdent "$type_param") e.etype expr.epos; run expr]); }

				| TBinop( (Ast.OpNotEq as op), e1, e2)
				| TBinop( (Ast.OpEq as op), e1, e2) when is_struct e1.etype || is_struct e2.etype ->
					let mk_ret e = match op with | Ast.OpNotEq -> { e with eexpr = TUnop(Ast.Not, Ast.Prefix, e) } | _ -> e in
					mk_ret { e with
						eexpr = TCall({
							eexpr = TField(run e1, FDynamic "Equals");
							etype = TFun(["obj1",false,t_dynamic;], basic.tbool);
							epos = e1.epos
						}, [ run e2 ])
					}

				| TBinop ( (Ast.OpEq as op), e1, e2 )
				| TBinop ( (Ast.OpNotEq as op), e1, e2 ) when is_cl e1.etype && not (in_runtime_class gen) ->
					let static = mk_static_field_access_infer (runtime_cl) "typeEq" e.epos [] in
					let ret = { e with eexpr = TCall(static, [run e1; run e2]); } in
					if op = Ast.OpNotEq then
						{ ret with eexpr = TUnop(Ast.Not, Ast.Prefix, ret) }
					else
						ret

				| _ -> Type.map_expr run e
		in
		gen.gsyntax_filters#add name (PCustom priority) run
end;;

let add_cast_handler gen =
	let basic = gen.gcon.basic in
	(*
		starting to set gtparam_cast.
	*)

	(* NativeArray: the most important. *)

	(*
		var new_arr = new NativeArray<TO_T>(old_arr.Length);
		var i = -1;
		while( i < old_arr.Length )
		{
			new_arr[i] = (TO_T) old_arr[i];
		}
	*)

	let native_arr_cl = get_cl ( get_type gen (["cs"], "NativeArray") ) in

	let get_narr_param t = match follow t with
		| TInst({ cl_path = (["cs"], "NativeArray") }, [param]) -> param
		| _ -> die "" __LOC__
	in

	let gtparam_cast_native_array e to_t =
		let old_param = get_narr_param e.etype in
		let new_param = get_narr_param to_t in

		let new_v = mk_temp "new_arr" to_t in
		let i = mk_temp "i" basic.tint in
		let old_len = mk_field_access gen e "Length" e.epos in
		let obj_v = mk_temp "obj" t_dynamic in
		let check_null = {eexpr = TBinop(Ast.OpNotEq, e, null e.etype e.epos); etype = basic.tbool; epos = e.epos} in
		let block = [
			{
				eexpr = TVar(
					new_v, Some( {
						eexpr = TNew(native_arr_cl, [new_param], [old_len] );
						etype = to_t;
						epos = e.epos
					} )
				);
				etype = basic.tvoid;
				epos = e.epos
			};
			{
				eexpr = TVar(i, Some( make_int gen.gcon.basic (-1) e.epos ));
				etype = basic.tvoid;
				epos = e.epos
			};
			{
				eexpr = TWhile(
					{
						eexpr = TBinop(
							Ast.OpLt,
							{ eexpr = TUnop(Ast.Increment, Ast.Prefix, mk_local i e.epos); etype = basic.tint; epos = e.epos },
							old_len
						);
						etype = basic.tbool;
						epos = e.epos
					},
					{ eexpr = TBlock [
						{
							eexpr = TVar(obj_v, Some (mk_cast t_dynamic { eexpr = TArray(e, mk_local i e.epos); etype = old_param; epos = e.epos }));
							etype = basic.tvoid;
							epos = e.epos
						};
						{
							eexpr = TIf({
								eexpr = TBinop(Ast.OpNotEq, mk_local obj_v e.epos, null e.etype e.epos);
								etype = basic.tbool;
								epos = e.epos
							},
							{
								eexpr = TBinop(
									Ast.OpAssign,
									{ eexpr = TArray(mk_local new_v e.epos, mk_local i e.epos); etype = new_param; epos = e.epos },
									mk_cast new_param (mk_local obj_v e.epos)
								);
								etype = new_param;
								epos = e.epos
							},
							None);
							etype = basic.tvoid;
							epos = e.epos
						}
					]; etype = basic.tvoid; epos = e.epos },
					Ast.NormalWhile
				);
				etype = basic.tvoid;
				epos = e.epos;
			};
			mk_local new_v e.epos
		] in
		{
			eexpr = TIf(
				check_null,
				{
					eexpr = TBlock(block);
					etype = to_t;
					epos = e.epos;
				},
				Some(null new_v.v_type e.epos)
			);
			etype = to_t;
			epos = e.epos;
		}
	in

	Hashtbl.add gen.gtparam_cast (["cs"], "NativeArray") gtparam_cast_native_array;
	Hashtbl.add gen.gtparam_cast (["haxe";"lang"], "Null") (fun e to_t -> mk_cast to_t e)
	(* end set gtparam_cast *)

let connecting_string = "?" (* ? see list here http://www.fileformat.info/info/unicode/category/index.htm and here for C# http://msdn.microsoft.com/en-us/library/aa664670.aspx *)
let default_package = "cs" (* I'm having this separated as I'm still not happy with having a cs package. Maybe dotnet would be better? *)
let strict_mode = ref false (* strict mode is so we can check for unexpected information *)

(* reserved c# words *)
let reserved = let res = Hashtbl.create 120 in
	List.iter (fun lst -> Hashtbl.add res lst ("@" ^ lst)) ["abstract"; "as"; "base"; "bool"; "break"; "byte"; "case"; "catch"; "char"; "checked"; "class";
		"const"; "continue"; "decimal"; "default"; "delegate"; "do"; "double"; "else"; "enum"; "event"; "explicit";
		"extern"; "false"; "finally"; "fixed"; "float"; "for"; "foreach"; "goto"; "if"; "implicit"; "in"; "int";
		"interface"; "internal"; "is"; "lock"; "long"; "namespace"; "new"; "null"; "object"; "operator"; "out"; "override";
		"params"; "private"; "protected"; "public"; "readonly"; "ref"; "return"; "sbyte"; "sealed"; "short"; "sizeof";
		"stackalloc"; "static"; "string"; "struct"; "switch"; "this"; "throw"; "true"; "try"; "typeof"; "uint"; "ulong";
		"unchecked"; "unsafe"; "ushort"; "using"; "virtual"; "volatile"; "void"; "while"; "add"; "ascending"; "by"; "descending";
		"dynamic"; "equals"; "from"; "get"; "global"; "group"; "into"; "join"; "let"; "on"; "orderby"; "partial";
		"remove"; "select"; "set"; "value"; "var"; "where"; "yield"; "await"];
	res

let dynamic_anon = mk_anon (ref Closed)

let rec get_class_modifiers meta cl_type cl_access cl_modifiers =
	match meta with
		| [] -> cl_type,cl_access,cl_modifiers
		| (Meta.Struct,[],_) :: meta -> get_class_modifiers meta "struct" cl_access cl_modifiers
		| (Meta.Protected,[],_) :: meta -> get_class_modifiers meta cl_type "protected" cl_modifiers
		| (Meta.Internal,[],_) :: meta -> get_class_modifiers meta cl_type "internal" cl_modifiers
		(* no abstract for now | (":abstract",[],_) :: meta -> get_class_modifiers meta cl_type cl_access ("abstract" :: cl_modifiers)
		| (":static",[],_) :: meta -> get_class_modifiers meta cl_type cl_access ("static" :: cl_modifiers) TODO: support those types *)
		| (Meta.Unsafe,[],_) :: meta -> get_class_modifiers meta cl_type cl_access ("unsafe" :: cl_modifiers)
		| _ :: meta -> get_class_modifiers meta cl_type cl_access cl_modifiers

let rec get_fun_modifiers meta access modifiers =
	match meta with
		| [] -> access,modifiers
		| (Meta.Private,[],_) :: meta -> get_fun_modifiers meta "private" modifiers
		| (Meta.Protected,[],_) :: meta -> get_fun_modifiers meta "protected" modifiers
		| (Meta.Internal,[],_) :: meta -> get_fun_modifiers meta "internal" modifiers
		| (Meta.ReadOnly,[],_) :: meta -> get_fun_modifiers meta access ("readonly" :: modifiers)
		| (Meta.Unsafe,[],_) :: meta -> get_fun_modifiers meta access ("unsafe" :: modifiers)
		| (Meta.Volatile,[],_) :: meta -> get_fun_modifiers meta access ("volatile" :: modifiers)
		| (Meta.Custom ("?prop_impl" | ":cs_event_impl"),[],_) :: meta -> get_fun_modifiers meta "private" modifiers
		| _ :: meta -> get_fun_modifiers meta access modifiers

let generate con =
	(try
		let gen = new_ctx con in
		let basic = con.basic in

		if Common.defined_value con Define.Dce = "no" then begin
			let m = { null_module with m_id = alloc_mid(); m_path = ["haxe";"lang"],"DceNo" } in
			let cl = mk_class m (["haxe";"lang"],"DceNo") null_pos in
			gen.gtypes_list <- (TClassDecl cl) :: gen.gtypes_list;
			Hashtbl.add gen.gtypes cl.cl_path (TClassDecl cl)
		end;

		(* make the basic functions in C# *)
		let type_cl = get_cl ( get_type gen (["System"], "Type")) in
		let basic_fns =
		[
			mk_class_field "Equals" (TFun(["obj",false,t_dynamic], basic.tbool)) true null_pos (Method MethNormal) [];
			mk_class_field "ToString" (TFun([], basic.tstring)) true null_pos (Method MethNormal) [];
			mk_class_field "GetHashCode" (TFun([], basic.tint)) true null_pos (Method MethNormal) [];
			mk_class_field "GetType" (TFun([], TInst(type_cl, []))) true null_pos (Method MethNormal) [];
		] in
		List.iter (fun cf -> gen.gbase_class_fields <- PMap.add cf.cf_name cf gen.gbase_class_fields) basic_fns;

		let native_arr_cl = get_cl ( get_type gen (["cs"], "NativeArray") ) in
		gen.gclasses.nativearray <- (fun t -> TInst(native_arr_cl,[t]));
		gen.gclasses.nativearray_type <- (function TInst(_,[t]) -> t | _ -> die "" __LOC__);
		gen.gclasses.nativearray_len <- (fun e p -> mk_field_access gen e "Length" p);

		let erase_generics = Common.defined gen.gcon Define.EraseGenerics in
		let fn_cl = get_cl (get_type gen (["haxe";"lang"],"Function")) in
		let null_t = if erase_generics then null_class else (get_cl (get_type gen (["haxe";"lang"],"Null")) ) in
		let runtime_cl = get_cl (get_type gen (["haxe";"lang"],"Runtime")) in
		let no_root = Common.defined gen.gcon Define.NoRoot in
		let change_id name = try
				Hashtbl.find reserved name
			with | Not_found ->
				let ret = String.concat "." (String.nsplit name "#") in
				List.hd (String.nsplit ret "`")
		in

		let change_clname n = change_id n in