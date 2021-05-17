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
open Common
open Type
open Codegen
open Gencommon

(* ******************************************* *)
(* SwitchToIf *)
(* ******************************************* *)
(*
	A syntax filter which changes switch expressions to if() else if() else if() ...

	Also it handles switches on native enums (which are not converted to classes) by
	rewriting the switch expression to what's supported directly by the targets.
*)
let name = "switch_to_if"
let priority = solve_deps name []

let rec simplify_expr e =
	match e.eexpr with
	| TParenthesis e | TMeta (_, e) -> simplify_expr e
	| _ -> e

let configure gen (should_convert:texpr->bool) =
	let basic = gen.gcon.basic in
	let rec run e =
		match e.eexpr with
		| TSwitch (cond, cases, default) when should_convert e ->
			let cond_etype, should_cache =
				match gen.gfollow#run_f cond.etype with
				| TAbstract ({ a_path = [], "Null" }, [t]) ->
					let rec take_off_nullable t =
						match gen.gfollow#run_f t with
						| TAbstract ({ a_path = [], "Null" }, [t]) -> take_off_nullable t
						| _ -> t
					in
					take_off_nullable t, true
				| _ ->
					cond.etype, false
			in

			if should_cache && not (should_convert { e with eexpr = TSwitch ({ cond with etype = cond_etype }, cases, default) }) then begin
				{ e with eexpr = TSwitch (mk_cast cond_etype (run cond), List.map (fun (cs,e) -> (List.map run cs, run e)) cases, Option.map run default) }
			end else begin
				let local, fst_block =
					match cond.eexpr, should_cache with
					| TLocal _, false ->
						cond, []
					| _ ->
						let va