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

type keyword =
	| Function
	| Class
	| Var
	| If
	| Else
	| While
	| Do
	| For
	| Break
	| Continue
	| Return
	| Extends
	| Implements
	| Import
	| Switch
	| Case
	| Default
	| Static
	| Public
	| Private
	| Try
	| Catch
	| New
	| This
	| Throw
	| Extern
	| Enum
	| In
	| Interface
	| Untyped
	| Cast
	| Override
	| Typedef
	| Dynamic
	| Package
	| Inline
	| Using
	| Null
	| True
	| False
	| Abstract
	| Macro
	| Final
	| Operator
	| Overload

type binop =
	| OpAdd
	| OpMult
	| OpDiv
	| OpSub
	| OpAssign
	| OpEq
	| OpNotEq
	| OpGt
	| OpGte
	| OpLt
	| OpLte
	| OpAnd
	| OpOr
	| OpXor
	| OpBoolAnd
	| OpBoolOr
	| OpShl
	| OpShr
	| OpUShr
	| OpMod
	| OpAssignOp of binop
	| OpInterval
	| OpArrow
	| OpIn
	| OpNullCoal

type unop =
	| Increment
	| Decrement
	| Not
	| Neg
	| NegBits
	| Spread

type string_literal_kind =
	| SDoubleQuotes
	| SSingleQuotes
	(* | SMarkup *)

type constant =
	| Int of string * string option
	| Float of string * string option
	| String of string * string_literal_kind
	| Ident of string
	| Regexp of string * string

type token =
	| Eof
	| Const of constant
	| Kwd of keyword
	| Comment of string
	| CommentLine of string
	| Binop of binop
	| Unop of unop
	| Semicolon
	| Comma
	| BrOpen
	| BrClose
	| BkOpen
	| BkClose
	| POpen
	| PClose
	| Dot
	| DblDot
	| QuestionDot
	| Arrow
	| IntInterval of string
	| Sharp of string
	| Question
	| At
	| Dollar of string
	| Spread

type unop_flag =
	| Prefix
	| Postfix

type while_flag =
	| NormalWhile
	| DoWhile

type quote_status =
	| NoQuotes
	| DoubleQuotes

type type_path = {
	tpackage : string list;
	tname : string;
	tparams : type_param_or_const list;
	tsub : string option;
}

and placed_type_path = type_path * pos

and type_param_or_const =
	| TPType of type_hint
	| TPExpr of expr

and complex_type =
	| CTPath of type_path
	| CTFunction of type_hint list * type_hint
	| CTAnonymous of class_field list
	| CTParent of type_hint
	| CTExtend of placed_type_path list * class_field list
	| CTOptional of type_hint
	| CTNamed of placed_name * type_hint
	| CTIntersection of type_hint list

and type_hint = complex_type * pos

and func = {
	f_params : type_param list;
	f_args : (placed_name * bool * metadata * type_hint option * expr option) list;
	f_type : type_hint option;
	f_expr : expr option;
}

and placed_name = string * pos

and function_kind =
	| FKAnonymous
	| FKNamed of placed_name * bool (* bool for `inline` *)
	| FKArrow

and display_kind =
	| DKCall
	| DKDot
	| DKStructure
	| DKMarked
	| DKPattern of bool

and efield_kind =
	| EFNormal
	| EFSafe

and expr_def =
	| EConst of constant
	| EArray of expr * expr
	| EBinop of binop * expr * expr
	| EField of expr * string * efield_kind
	| EParenthesis of expr
	| EObjectDecl of ((string * pos * quote_status) * expr) list
	| EArrayDecl of expr list
	| ECall of expr * expr list
	| ENew of placed_type_path * expr list
	| EUnop of unop * unop_flag * expr
	| EVars of evar list
	| EFunction of function_kind * func
	| EBlock of expr list
	| EFor of expr * expr
	| EIf of expr * expr * expr option
	| EWhile of expr * expr * while_flag
	| ESwitch of expr * (expr list * expr option * expr option * pos) list * (expr option * pos) option
	| ETry of expr * (placed_name * type_hint option * expr * pos) list
	| EReturn of expr option
	| EBreak
	| EContinue
	| EUntyped of expr
	| EThrow of expr
	| ECast of expr * type_hint option
	| EIs of expr * type_hint
	| EDisplay of expr * display_kind
	| ETernary of expr * expr * expr
	| ECheckType of expr * type_hint
	| EMeta of metad