open Globals
open Ast
open Type
open Typecore
open Error

type access_kind =
	(* Access is not possible or allowed. *)
	| AKNo of access_kind * pos
	(* Access on arbitrary expression. *)
	| AKExpr of texpr
	(* Safe navigation access chain *)
	| AKSafeNav of safe_nav_access
	(* Access on non-property field. *)
	| AKField of field_access
	(* Access on propert