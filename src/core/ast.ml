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
	| EMeta of metadata_entry * expr

and expr = expr_def * pos

and type_param = {
	tp_name : placed_name;
	tp_params :	type_param list;
	tp_constraints : type_hint option;
	tp_default : type_hint option;
	tp_meta : metadata;
}

(**
	This structure represents a documentation comment of a symbol.

	Use `Ast.get_doc_text` to generate a final user-readable text for a doc_block.
*)
and doc_block = {
	(** Contains own docs written nearby the symbol in Haxe code *)
	doc_own: string option;
	(**
		This field is for docs pointed by @:inheritDoc meta.

		It's populated with `InheritDoc.build_*` functions.
		Each string in this list is compiled of a doc a single @:inheritDoc points to.

		E.g. calling `InheritDoc.build_class_field_doc` for `field4` (from sample below)
		will produce `doc_inherited = ["Own field3 doc"; "Own field2 doc\nOwn field1 doc"]`.

		Sample:
		```
		class MyClass {

			/** Own field1 doc */
			function field1();

			/** Own field2 doc */
			@:inheritDoc(MyClass.field1) function field2();

			/** Own field3 doc */
			function field2();

			/** Own field4 doc */
			@:inheritDoc(MyClass.field3)
			@:inheritDoc(MyClass.field2)
			function field4();
		}
		```
	*)
	mutable doc_inherited: string list;
}

and documentation = doc_block option

and metadata_entry = (Meta.strict_meta * expr list * pos)
and metadata = metadata_entry list

and access =
	| APublic
	| APrivate
	| AStatic
	| AOverride
	| ADynamic
	| AInline
	| AMacro
	| AFinal
	| AExtern
	| AAbstract
	| AOverload

and placed_access = access * pos

and class_field_kind =
	| FVar of type_hint option * expr option
	| FFun of func
	| FProp of placed_name * placed_name * type_hint option * expr option

and class_field = {
	cff_name : placed_name;
	cff_doc : documentation;
	cff_pos : pos;
	mutable cff_meta : metadata;
	mutable cff_access : placed_access list;
	mutable cff_kind : class_field_kind;
}

and evar = {
	ev_name : placed_name;
	ev_final : bool;
	ev_static : bool;
	ev_type : type_hint option;
	ev_expr : expr option;
	ev_meta : metadata;
}

(* TODO: should we introduce CTMono instead? *)
let ct_mono = CTPath { tpackage = ["$"]; tname = "_hx_mono"; tparams = []; tsub = None }

type enum_flag =
	| EPrivate
	| EExtern

type class_flag =
	| HInterface
	| HExtern
	| HPrivate
	| HExtends of placed_type_path
	| HImplements of placed_type_path
	| HFinal
	| HAbstract

type abstract_flag =
	| AbPrivate
	| AbFrom of type_hint
	| AbTo of type_hint
	| AbOver of type_hint
	| AbExtern
	| AbEnum

type enum_constructor = {
	ec_name : placed_name;
	ec_doc : documentation;
	ec_meta : metadata;
	ec_args : (string * bool * type_hint) list;
	ec_pos : pos;
	ec_params : type_param list;
	ec_type : type_hint option;
}

type ('a,'b) definition = {
	d_name : placed_name;
	d_doc : documentation;
	d_params : type_param list;
	d_meta : metadata;
	d_flags : 'a list;
	d_data : 'b;
}

type import_mode =
	| INormal
	| IAsName of placed_name
	| IAll

type import = placed_name list * import_mode

type type_def =
	| EClass of (class_flag, class_field list) definition
	| EEnum of (enum_flag, enum_constructor list) definition
	| ETypedef of (enum_flag, type_hint) definition
	| EAbstract of (abstract_flag, class_field list) definition
	| EStatic of (placed_access, class_field_kind) definition
	| EImport of import
	| EUsing of placed_name list

type type_decl = type_def * pos

type package = string list * type_decl list

let mk_type_path ?(params=[]) ?sub (pack,name) =
	if name = "" then
		raise (Invalid_argument "Empty module name is not allowed");
	{ tpackage = pack; tname = name; tsub = sub; tparams = params; }

let mk_evar ?(final=false) ?(static=false) ?(t:type_hint option) ?eo ?(meta=[]) name =
	{
		ev_name = name;
		ev_final = final;
		ev_static = static;
		ev_type = t;
		ev_expr = eo;
		ev_meta = meta;
	}

let is_lower_ident i =
	if String.length i = 0 then
		raise (Invalid_argument "Identifier name must not be empty")
	else
		let rec loop p =
			match String.unsafe_get i p with
			| 'a'..'z' -> true
			| '_' -> p + 1 >= String.length i || loop (p + 1)
			| _ -> false
		in
		loop 0

let pos = snd

let doc_from_string s = Some { doc_own = Some s; doc_inherited = []; }

let doc_from_string_opt = Option.map (fun s -> { doc_own = Some s; doc_inherited = []; })

(**
	Generates full doc block text out of `doc_block` structure
	by concatenating `d.doc_own` and all entries of `d.doc_inherited` with new lines
	in between.
*)
let gen_doc_text d =
	let docs =
		match d.doc_own with
		| Some s -> s :: d.doc_inherited
		| None -> d.doc_inherited
	in
	String.concat "\n" docs


let gen_doc_text_opt = Option.map gen_doc_text

let get_own_doc_opt = Option.map_default (fun d -> d.doc_own) None

let rec is_postfix (e,_) op = match op with
	| Increment | Decrement | Not -> true
	| Neg | NegBits | Spread -> false

let base_class_name = snd

let punion p p2 =
	{
		pfile = p.pfile;
		pmin = min p.pmin p2.pmin;
		pmax = max p.pmax p2.pmax;
	}

let rec punion_el el = match el with
	| [] ->
		null_pos
	| (_,p) :: [] ->
		p
	| (_,p) :: el ->
		punion p (punion_el el)

let parse_path s =
	match List.rev (ExtString.String.nsplit s ".") with
	| [] -> [],"" (* This is how old extlib behaved. *)
	| x :: l -> List.rev l, x

let s_constant = function
	| Int (s, None) -> s
	| Int (s, Some suffix) -> s ^ suffix
	| Float (s, None) -> s
	| Float (s, Some suffix) -> s ^ suffix
	| String(s,qs) ->
		begin match qs with
		| SDoubleQuotes -> "\"" ^ StringHelper.s_escape s ^ "\""
		| SSingleQuotes -> "\"" ^ StringHelper.s_escape s ^ "\""
		end
	| Ident s -> s
	| Regexp (r,o) -> "~/" ^ r ^ "/"

let s_access = function
	| APublic -> "public"
	| APrivate -> "private"
	| AStatic -> "static"
	| AOverride -> "override"
	| ADynamic -> "dynamic"
	| AInline -> "inline"
	| AMacro -> "macro"
	| AFinal -> "final"
	| AExtern -> "extern"
	| AAbstract -> "abstract"
	| AOverload -> "overload"

let s_placed_access (a,_) = s_access a

let s_keyword = function
	| Function -> "function"
	| Class -> "class"
	| Static -> "static"
	| Var -> "var"
	| If -> "if"
	| Else -> "else"
	| While -> "while"
	| Do -> "do"
	| For -> "for"
	| Break -> "break"
	| Return -> "return"
	| Continue -> "continue"
	| Extends -> "extends"
	| Implements -> "implements"
	| Import -> "import"
	| Switch -> "switch"
	| Case -> "case"
	| Default -> "default"
	| Private -> "private"
	| Public -> "public"
	| Try -> "try"
	| Catch -> "catch"
	| New -> "new"
	| This -> "this"
	| Throw -> "throw"
	| Extern -> "extern"
	| Enum -> "enum"
	| In -> "in"
	| Interface -> "interface"
	| Untyped -> "untyped"
	| Cast -> "cast"
	| Override -> "override"
	| Typedef -> "typedef"
	| Dynamic -> "dynamic"
	| Package -> "package"
	| Inline -> "inline"
	| Using -> "using"
	| Null -> "null"
	| True -> "true"
	| False -> "false"
	| Abstract -> "abstract"
	| Macro -> "macro"
	| Final -> "final"
	| Operator -> "operator"
	| Overload -> "overload"

let rec s_binop = function
	| OpAdd -> "+"
	| OpMult -> "*"
	| OpDiv -> "/"
	| OpSub -> "-"
	| OpAssign -> "="
	| OpEq -> "=="
	| OpNotEq -> "!="
	| OpGte -> ">="
	| OpLte -> "<="
	| OpGt -> ">"
	| OpLt -> "<"
	| OpAnd -> "&"
	| OpOr -> "|"
	| OpXor -> "^"
	| OpBoolAnd -> "&&"
	| OpBoolOr -> "||"
	| OpShr -> ">>"
	| OpUShr -> ">>>"
	| OpShl -> "<<"
	| OpMod -> "%"
	| OpAssignOp op -> s_binop op ^ "="
	| OpInterval -> "..."
	| OpArrow -> "=>"
	| OpIn -> " in "
	| OpNullCoal -> "??"

let s_unop = function
	| Increment -> "++"
	| Decrement -> "--"
	| Not -> "!"
	| Neg -> "-"
	| NegBits -> "~"
	| Spread -> "..."

let s_token = function
	| Eof -> "<end of file>"
	| Const c -> s_constant c
	| Kwd k -> s_keyword k
	| Comment s -> "/*"^s^"*/"
	| CommentLine s -> "//"^s
	| Binop o -> s_binop o
	| Unop o -> s_unop o
	| Semicolon -> ";"
	| Comma -> ","
	| BkOpen -> "["
	| BkClose -> "]"
	| BrOpen -> "{"
	| BrClose -> "}"
	| POpen -> "("
	| PClose -> ")"
	| Dot -> "."
	| DblDot -> ":"
	| QuestionDot -> "?."
	| Arrow -> "->"
	| IntInterval s -> s ^ "..."
	| Sharp s -> "#" ^ s
	| Question -> "?"
	| At -> "@"
	| Dollar v -> "$" ^ v
	| Spread -> "..."

exception Invalid_escape_sequence of char * int * (string option)

let efield (e,s) =
	EField(e,s,EFNormal)

let unescape s =
	let b = Buffer.create 0 in
	let rec loop esc i =
		if i = String.length s then
			()
		else
			let c = s.[i] in
			let fail msg = raise (Invalid_escape_sequence(c,i,msg)) in
			if esc then begin
				let inext = ref (i + 1) in
				(match c with
				| 'n' -> Buffer.add_char b '\n'
				| 'r' -> Buffer.add_char b '\r'
				| 't' -> Buffer.add_char b '\t'
				| '"' | '\'' | '\\' -> Buffer.add_char b c
				| '0'..'3' ->
					let u = (try (int_of_string ("0o" ^ String.sub s i 3)) with _ -> fail None) in
					if u > 127 then
						fail (Some ("Values greater than \\177 are not allowed. Use \\u00" ^ (Printf.sprintf "%02x" u) ^ " instead."));
					Buffer.add_char b (char_of_int u);
					inext := !inext + 2;
				| 'x' ->
					let fail_no_hex () = fail (Some "Must be followed by a hexadecimal sequence.") in
					let hex = try String.sub s (i+1) 2 with _ -> fail_no_hex () in
					let u = (try (int_of_string ("0x" ^ hex)) with _ -> fail_no_hex ()) in
					if u > 127 then
						fail (Some ("Values greater than \\x7f are not allowed. Use \\u00" ^ hex ^ " instead."));
					Buffer.add_char b (char_of_int u);
					inext := !inext + 2;
				| 'u' ->
					let fail_no_hex () = fail (Some "Must be followed by a hexadecimal sequence enclosed in curly brackets.") in
					let (u, a) =
						try
							(int_of_string ("0x" ^ String.sub s (i+1) 4), 4)
						with _ -> try
							assert (s.[i+1] = '{');
							let l = String.index_from s (i+3) '}' - (i+2) in
							let u = int_of_string ("0x" ^ String.sub s (i+2) l) in
							if u > 0x10FFFF then
								fail (Some "Maximum allowed value for unicode escape sequence is \\u{10FFFF}");
							(u, l+2)
						with
							| Invalid_escape_sequence (c,i,msg) as e -> raise e
							| _ -> fail_no_hex ()
					in
					if u >= 0xD800 && u < 0xE000 then
						fail (Some "UTF-16 surrogates are not allowed in strings.");
					UTF8.add_uchar b (UCharExt.uchar_of_int u);
					inext := !inext + a;
				| _ ->
					fail None);
				loop false !inext;
			end else
				match c with
				| '\\' -> loop true (i + 1)
				| c ->
					Buffer.add_char b c;
					loop false (i + 1)
	in
	loop false 0;
	Buffer.contents b

let map_expr loop (e,p) =
	let opt f o =
		match o with None -> None | Some v -> Some (f v)
	in
	let rec tparam = function
		| TPType t -> TPType (type_hint t)
		| TPExpr e -> TPExpr (loop e)
	and cfield f =
		{ f with cff_kind = (match f.cff_kind with
			| FVar (t,e) ->
				let t = opt type_hint t in
				let e = opt loop e in
				FVar (t,e)
			| FFun f -> FFun (func f)
			| FProp (get,set,t,e) ->
				let t = opt type_hint t in
				let e = opt loop e in
				FProp (get,set,t,e))
		}
	and type_hint (t,p) = (match t with
		| CTPath t -> CTPath { t with tparams = List.map tparam t.tparams }
		| CTFunction (cl,c) ->
			let cl = List.map type_hint cl in
			let c = type_hint c in
			CTFunction (cl,c)
		| CTAnonymous fl -> CTAnonymous (List.map cfield fl)
		| CTParent t -> CTParent (type_hint t)
		| CTExtend (tl,fl) ->
			let tl = List.map tpath tl in
			let fl = List.map cfield fl in
			CTExtend (tl,fl)
		| CTOptional t -> CTOptional (type_hint t)
		| CTNamed (n,t) -> CTNamed (n,type_hint t)
		| CTIntersection tl -> CTIntersection(List.map type_hint tl)
		),p
	and tparamdecl t =
		let constraints = opt type_hint t.tp_constraints in
		let default = opt type_hint t.tp_default in
		let params = List.map tparamdecl t.tp_params in
		{ tp_name = t.tp_name; tp_constraints = constraints; tp_default = default; tp_params = params; tp_meta = t.tp_meta }
	and func f =
		let params = List.map tparamdecl f.f_params in
		let args = List.map (fun (n,o,m,t,e) ->
			let t = opt type_hint t in
			let e = opt loop e in
			n,o,m,t,e
		) f.f_args in
		let t = opt type_hint f.f_type in
		let e = opt loop f.f_expr in
		{
			f_params = params;
			f_args = args;
			f_type = t;
			f_expr = e;
		}
	and tpath (t,p) = { t with tparams = List.map tparam t.tparams },p
	in
	let e = (match e with
	| EConst _ -> e
	| EArray (e1,e2) ->
		let e1 = loop e1 in
		let e2 = loop e2 in
		EArray (e1,e2)
	| EBinop (op,e1,e2) ->
		let e1 = loop e1 in
		let e2 = loop e2 in
		EBinop (op,e1,e2)
	| EField (e,f,efk) -> EField (loop e, f, efk)
	| EParenthesis e -> EPare