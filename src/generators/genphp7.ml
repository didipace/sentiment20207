(**
	Compatible with PHP 7.0+
*)

open Ast
open Type
open Common
open Meta
open Globals
open Sourcemaps

(**
	Escape string for constant strings generation.
	Copy-pasted from genphp.
*)
let escape_bin s =
	let b = Buffer.create 0 in
	for i = 0 to String.length s - 1 do
		match Char.code (String.unsafe_get s i) with
		| c when c = Char.code('\\') || c = Char.code('"') || c = Char.code('$') ->
			Buffer.add_string b "\\";
			Buffer.add_char b (Char.chr c)
		| c when c < 32 ->
			Buffer.add_string b (Printf.sprintf "\\x%.2X" c)
		| c ->
			Buffer.add_char b (Char.chr c)
	done;
	Buffer.contents b

(**
	Write resources passed to compiler via `-resource` flag
	Copy-pasted from genphp
*)
let write_resource dir name data =
	let rdir = dir ^ "/res" in
	if not (Sys.file_exists dir) then Unix.mkdir dir 0o755;
	if not (Sys.file_exists rdir) then Unix.mkdir rdir 0o755;
	let name = Codegen.escape_res_name name false in
	let ch = open_out_bin (rdir ^ "/" ^ name) in
	output_string ch data;
	close_out ch

(**
	Copy file from `src` to `dst`.
	If `dst` exists it will be overwritten.
*)
let copy_file src dst =
	let buffer_size = 8192 in
	let buffer = String.create buffer_size in
	let fd_in = Unix.openfile src [O_RDONLY] 0 in
	let fd_out = Unix.openfile dst [O_WRONLY; O_CREAT; O_TRUNC] 0o644 in
	let rec copy_loop () =
		match Unix.read fd_in buffer 0 buffer_size with
			|  0 -> ()
			| r -> ignore (Unix.write fd_out buffer 0 r); copy_loop ()
	in
	copy_loop ();
	Unix.close fd_in;
	Unix.close fd_out
(**
	Splits `"path/to/file"` into `["path"; "to"; "file"]`
*)
let split_file_path path =
	if Globals.is_windows then
		(Str.split (Str.regexp "[/\\]") path)
	else
		(Str.split (Str.regexp "/") path)

type used_type = {
	ut_alias : string;
	ut_type_path : (string list * string)
}

type php_generator_context = {
	pgc_common : Common.context;
	(** Do not add comments with Haxe positions before each line of generated php code *)
	pgc_skip_line_directives : bool;
	(** The value of `-D php-prefix=value` split by dots *)
	pgc_prefix : string list;
	(** php.Boot *)
	pgc_boot : tclass;
	(** see type_name_used_in_namespace *)
	pgc_namespaces_types_cache : (string list, string) Hashtbl.t;
	(**
		List of anon structures declarations found during generating current php file.
		The key is a list of fields names.
		The value is an auto-generated name for the class representing that anon.
	*)
	pgc_anons : (string list, string) Hashtbl.t;
	(** a buffer to write to the bottom of the current php file (but before "Boot::registerClass()" and "::_hx_init()" calls) *)
	pgc_bottom_buffer : Buffer.t;
}

(**
	Reset the state of the context between before a generating a php file.
*)
let reset_context ctx =
	Buffer.clear ctx.pgc_bottom_buffer;
	Hashtbl.clear ctx.pgc_anons

(**
	Get list of keys in Hashtbl
*)
let hashtbl_keys tbl = Hashtbl.fold (fun key _ lst -> key :: lst) tbl []

(**
	@return List of items in `list1` which `list2` does not contain
*)
let diff_lists list1 list2 = List.filter (fun x -> not (List.mem x list2)) list1

(**
	@return List of items in `list1` which `list2` does contain too
*)
let intersect_lists list1 list2 = List.filter (fun x -> List.mem x list2) list1

(**
	Type path of `php.Boot`
*)
let boot_type_path = (["php"], "Boot")
(**
	Type path of the base class for all enums: `php.Boot.HxEnum`
*)
let hxenum_type_path = (["php"; "_Boot"], "HxEnum")
(**
	Type path of the implementation class for `Class<Dynamic>`
*)
let hxclass_type_path = (["php"; "_Boot"], "HxClass")
(**
	Type path of the implementation class for `String`
*)
let hxstring_type_path = (["php"; "_Boot"], "HxString")
(**
	Type path of the special implementation class for `String`
	which is used when Dynamic value is suspected to be a string
*)
let hxdynamicstr_type_path = (["php"; "_Boot"], "HxDynamicStr")
(**
	Type path of the implementation class for anonymous objects
*)
let hxanon_type_path = (["php"; "_Boot"], "HxAnon")
(**
	Type path of the implementation class for closures
*)
let hxclosure_type_path = (["php"; "_Boot"], "HxClosure")
(**
	Type path for special PHP extern class to support specific language expressions
*)
let syntax_type_path = (["php"], "Syntax")
(**
	Special abstract which enables passing function arguments and return value by reference
*)
let ref_type_path = (["php"], "Ref")
(**
	Type path of the implementation class for `Array<T>`
*)
let array_type_path = ([], "Array")
(**
	Type path of the implementation class for `Array<T>`
*)
let native_array_type_path = (["php"], "NativeArray")
(**
	Type path of the `Void`
*)
let void_type_path = ([], "Void")
(**
	Type path of the `Bool`
*)
let bool_type_path = ([], "Bool")
(**
	Type path of the `Std`
*)
let std_type_path = ([], "Std")

(**
	The name of a file with polyfills for some functions which are not available in PHP 7.0
*)
let polyfills_file = "_polyfills.php"

let php_keywords_list =
	["__halt_compiler"; "abstract"; "and"; "array"; "as"; "break"; "callable"; "case"; "catch"; "class";
	"clone"; "const"; "continue"; "declare"; "default"; "die"; "do"; "echo"; "else"; "elseif"; "empty";
	"enddeclare"; "endfor"; "endforeach"; "endif"; "endswitch"; "endwhile"; "eval"; "exit"; "extends"; "final";
	"finally"; "for"; "foreach"; "function"; "global"; "goto"; "if"; "implements"; "include"; "include_once";
	"instanceof"; "insteadof"; "interface"; "isset"; "list"; "namespace"; "new"; "or"; "print"; "private";
	"protected"; "public"; "require"; "require_once"; "return"; "static"; "switch"; "throw"; "trait"; "try";
	"unset"; "use"; "var"; "while"; "xor"; "yield"; "__class__"; "__dir__"; "__file__"; "__function__"; "__line__";
	"__method__"; "__trait__"; "__namespace__"; "int"; "float"; "bool"; "string"; "true"; "false"; "null"; "parent";
	"void"; "iterable"; "object"; "fn"]

let php_keywords_tbl = begin
	let tbl = Hashtbl.create 100 in
	List.iter (fun kwd -> Hashtbl.add tbl kwd ()) php_keywords_list;
	tbl
end

(**
	Check if specified string is a reserved word in PHP
*)
let is_keyword str = Hashtbl.mem php_keywords_tbl (String.lowercase str)

(**
	Check if specified type is php.NativeArray
*)
let is_native_array_type t = match follow t with TAbstract ({ a_path = tp }, _) -> tp = native_array_type_path | _ -> false

(**
	If `name` is not a reserved word in PHP then `name` is returned as-is.
	Otherwise this method returns another string, which can be used instead of `name`
*)
let get_real_name name = if is_keyword name then name ^ "_hx" else name

(**
	Returns local variable name free of risk to collide with superglobals like $_SERVER or $_GET
*)
let vname name =
	match name with
	| "GLOBALS" | "_SERVER" | "_GET" | "_POST" | "_FILES" | "_COOKIE"
	| "_SESSION" | "_REQUEST" | "_ENV" -> name ^ "_hx_"
	| _ -> name

(**
	If `path` contains some reserved in PHP words, they will be replaced with allowed words.
*)
let get_real_path path = List.map get_real_name path

(**
	Resolve real type (bypass abstracts and typedefs)
*)
let rec follow = Abstract.follow_with_abstracts

(**
	Adds packages specified by `-D php-prefix` to `type_path`.
	E.g. if `-D php-prefix=some.sub` and `type_path` is `(["pack"], "MyClass")`, then this function
	will return `(["some", "sub", "pack"], "MyClass")`
*)
let add_php_prefix ctx type_path =
	match type_path with
		| (pack, name) -> (ctx.pgc_prefix @ pack, name)

(**
	If `expr` is a TCast or TMeta, then returns underlying expression (recursively bypassing nested casts).
	Otherwise returns `expr` as is.
*)
let rec reveal_expr expr =
	match expr.eexpr with
		| TCast (e, _) -> reveal_expr e
		| TMeta (_, e) -> reveal_expr e
		| _ -> expr

(**
	If `expr` is a TCast or TMeta or TParenthesis, then returns underlying expression (recursively bypassing nested casts and parenthesis).
	Otherwise returns `expr` as is.
*)
let rec reveal_expr_with_parenthesis expr =
	match expr.eexpr with
		| TCast (e, _) -> reveal_expr_with_parenthesis e
		| TMeta (_, e) -> reveal_expr_with_parenthesis e
		| TParenthesis e -> reveal_expr_with_parenthesis e
		| _ -> expr

(**
	Get string representation of specified position in Haxe code.
*)
let stringify_pos pos = Lexer.get_error_pos (Printf.sprintf "%s:%d:") pos

(**
	@return Error message with position information
*)
let error_message pos message = (stringify_pos pos) ^ ": " ^ message

(**
	Terminates compiler process and prints user-friendly instructions about filing an issue in compiler repo.
*)
let fail ?msg p = Globals.die (Option.default "" msg) ~p

(**
	Check if `target` is a `Dynamic` type
*)
let rec is_dynamic_type (target:Type.t) = match follow target with TDynamic _ -> true | _ -> false

(**
	Check if `target` is `php.Ref`
*)
let is_ref (target:Type.t) = match target with TType ({ t_path = type_path }, _) -> type_path = ref_type_path | _ -> false

(**
	Check if `field` is a `dynamic function`
*)
let rec is_dynamic_method (field:tclass_field) =
	match field.cf_kind with
		| Method MethDynamic -> true
		| _ -> false

(**
	Check if specified expression is of `Dynamic` type
*)
let is_dynamic expr = is_dynamic_type expr.etype

(**
	Check if specified expression is of `Int` type
*)
let is_int expr = match follow expr.etype with TAbstract ({ a_path = ([], "Int") }, _) -> true | _ -> false

(**
	Check if specified expression is of `Float` type
*)
let is_float expr = ExtType.is_float (follow expr.etype)

(**
	Check if specified expression is of String type
*)
let is_string expr = ExtType.is_string (follow expr.etype)

(**
	Check if specified type is Array
*)
let is_array_type t = match follow t with TInst ({ cl_path = ([], "Array") }, _) -> true | _ -> false

(**
	Check if specified type is haxe.Rest
*)
let is_rest_type t = ExtType.is_rest (Type.follow t)

(**
	Check if specified type represents a function
*)
let is_function_type t = match follow t with TFun _ -> true | _ -> false

(**
	Check if `expr` is an access to a method of special `php.PHP` class
*)
let is_syntax_extern expr =
	match expr.eexpr with
		| TField ({ eexpr = TTypeExpr (TClassDecl { cl_path = path }) }, _) -> path = syntax_type_path
		| _ -> false

(**
	Check if specified type is actually a generic parameter
*)
let is_generic_parameter (target:Type.t) =
	match follow target with
		| TInst ({ cl_kind = KTypeParameter _ }, _) -> true
		| _ -> false

(**
	Check if `target` type cannot be clarified on compilation
*)
let is_unknown_type (target:Type.t) = is_dynamic_type target || is_generic_parameter target

(**
	@return `expr` wrapped in parenthesis
*)
let parenthesis expr = {eexpr = TParenthesis expr; etype = expr.etype; epos = expr.epos}

(**
	Check if `current` binary should be surrounded with parenthesis
*)
let need_parenthesis_for_binop current parent =
	if current = parent && current != OpNotEq && current != OpEq then
		false
	else
		match (current, parent) with
			| (_, OpAssign) -> false
			| (_, OpAssignOp _) -> false
			| (OpAdd, OpSub) -> false
			| (OpSub, OpAdd) -> false
			| (OpMult, OpDiv) -> false
			| (OpDiv, OpMult) -> false
			| (OpMult, OpAdd) -> false
			| (OpMult, OpSub) -> false
			| (OpDiv, OpAdd) -> false
			| (OpDiv, OpSub) -> false
			| _ -> true

(**
	Check if specified expression may require dereferencing if used as "temporary expression"
*)
let needs_dereferencing for_assignment expr =
	let is_create target_expr =
		match (reveal_expr_with_parenthesis target_expr).eexpr with
			| TNew _ -> for_assignment
			| TArrayDecl _ -> for_assignment
			| TObjectDecl _ -> for_assignment
			| TConst TNull -> true
			| TIf _ -> true
			(* some of `php.Syntax` methods *)
			| TCall ({ eexpr = TField (_, FStatic ({ cl_path = syntax_type_path }, { cf_name = name })) }, _) ->
				(match name with
					| "codeDeref" | "coalesce" | "assocDecl" | "arrayDecl" -> for_assignment
					| _ -> false
				)
			| _ -> false
	in
	match (reveal_expr expr).eexpr with
		| TField (target_expr, _) -> is_create target_expr
		| TArray (target_expr, _) -> is_create target_expr
		| _ -> false

(**
	Check if the value of `expr` needs to be stored to a temporary variable to be
	reused.
*)
let rec needs_temp_var expr =
	match (reveal_expr_with_parenthesis expr).eexpr with
		| TConst _ | TLocal _ -> false
		| TField (target, FInstance _) | TField (target, FStatic _) -> needs_temp_var target
		| TArray (target, index) -> needs_temp_var target || needs_temp_var index
		| _ -> true

(**
	@return (arguments_list, return_type)
*)
let get_function_signature (field:tclass_field) : (string * bool * Type.t) list * Type.t =
	match follow field.cf_type with
		| TFun (args, return_type) -> (args, return_type)
		| _ -> fail field.cf_pos __LOC__

(**
	Check if `target` is 100% guaranteed to be a scalar type in PHP.
	Inversion of `is_sure_scalar` does not guarantee `target` is not scalar.
*)
let is_sure_scalar (target:Type.t) =
	match follow target with
		| TInst ({ cl_path = ([], "String") }, _) -> true
		| TAbstract ({ a_path = ([], ("Int" | "Float" | "Bool"))}, _) -> true
		| _ -> false

(**
	Indicates if `expr` has to be wrapped into parentheses to be called.
*)
let rec needs_parenthesis_to_call expr =
	match expr.eexpr with
		| TParenthesis _ -> false
		| TCast (e, None)
		| TMeta (_, e) -> needs_parenthesis_to_call e
		| TNew _
		| TObjectDecl _
		| TArrayDecl _
		| TField (_, FClosure (_,_))
		| TField (_, FStatic (_, { cf_kind = Var _ }))
		| TField (_, FInstance (_, _, { cf_kind = Var _ })) -> true
		(* | TField (_, FAnon { cf_kind = Var _ }) -> true *) (* Sometimes we get anon access to non-anonymous objects *)
		| _ -> false

(**
	Check if specified unary operation modifies value in place
*)
let is_modifying_unop op =
	match op with
		| Increment
		| Decrement -> true
		| _ -> false

(**
	Check if specified binary operation contains assignment
*)
let is_assignment_binop op =
	match op with
		| OpAssign
		| OpAssignOp _ -> true
		| _ -> false

(**
	Indicates whether `expr` is a field access which should be generated as global namespace function
*)
let is_php_global expr =
	match expr.eexpr with
		| TField (_, FStatic (c, _)) when (has_class_flag c CExtern) -> c.cl_path = ([],"") || Meta.has Meta.PhpGlobal c.cl_meta
		| _ -> false

(**
	Indicates whether `expr` is a field access which should be generated as class constant access
*)
let is_php_class_const expr =
	match expr.eexpr with
		| TField (_, FStatic (c, { cf_meta = meta; cf_kind = Var _ })) when (has_class_flag c CExtern) ->
			Meta.has Meta.PhpClassConst meta
		| _ -> false

(**
	Check if specified enum constructor has arguments
*)
let is_enum_constructor_with_args (constructor:tenum_field) =
	match follow constructor.ef_type with
		| TFun _ -> true
		| _ -> false

(**
	Check if `target` is 100% guaranteed to be or extend an extern class.
	Inversion of `sure_extends_extern` does not guarantee `target` does not extend an extern class.
*)
let rec sure_extends_extern (target:Type.t) =
	match follow target with
		| TInst ({ cl_path = ([], "String") }, _) -> false
		| TInst (c, _) when (has_class_flag c CExtern) -> true
		| TInst ({ cl_super = Some (tsuper, params) }, _) -> sure_extends_extern (TInst (tsuper,params))
		| _ -> false

(**
	@param path Something like [ "/some/path/first_dir_to_create"; "nested_level1"; "nested_level2" ]
	@return String representation of created path (E.g. "/some/path/first_dir_to_create/nested_level1/nested_level2")
*)
let create_dir_recursive (path:string list) =
	let rec create dir nested_dirs =
		let dir = Path.remove_trailing_slash dir in
		if not (Sys.file_exists dir) then (Unix.mkdir dir 0o755);
		match nested_dirs with
			| [] -> dir
			| next :: rest -> create (dir ^ "/" ^ next) rest
	in
	match path with
		| [] -> "";
		| root :: rest ->
			create root rest

(**
	@return String representation of specified type path. E.g. returns "\example\Test" for (["example"], "Test")
*)
let get_full_type_name ?(escape=false) ?(omit_first_slash=false) (type_path:path) =
	let name =
		match type_path with
			| ([], type_name) ->
				if omit_first_slash then
					type_name
				else
					"\\" ^ type_name
			| (module_path, type_name) ->
				let parts =
					if omit_first_slash then
						get_real_path module_path
					else
						"" :: get_real_path module_path
				in
				(String.concat "\\" parts) ^ "\\" ^ type_name
	in
	if escape then
		String.escaped name
	else
		name

(**
	@return Short type name. E.g. returns "Test" for (["example"], "Test")
*)
let get_type_name (type_path:path) = snd type_path

(**
	@return E.g. returns ["example"] for (["example"], "Test")
*)
let get_module_path (type_path:path) = fst type_path

(**
	@return PHP visibility keyword.
*)
let get_visibility (meta:metadata) = if Meta.has Meta.Protected meta then "protected" else "public"

(**
	Writes arguments list to output buffer
*)
let rec write_args (str_writer:string->unit) arg_writer (args:'a list) =
	match args with
		| [] -> ()
		| [arg] -> arg_writer arg
		| arg :: rest ->
			arg_writer arg;
			str_writer ", ";
			write_args str_writer arg_writer rest

(**
	PHP 8 doesn't allow mandatory arguments after optional arguments.
	This function makes optional arguments mandatory from left to right
	unless there are no more mandatory arguments left to the end of args list.

	E.g `(a:String = null, b:Int, c:Bool = false)` is changed into `(a:String, b:Int, c:Bool = false)`
*)
let fix_optional_args is_optional to_mandatory args =
	let rec find_last_mandatory args i result =
		match args with
		| [] ->
			result
		| a :: args ->
			find_last_mandatory args (i + 1) (if is_optional a then result else i)
	in
	let last_mandatory = find_last_mandatory args 0 (-1) in
	List.mapi (fun i a -> if i <= last_mandatory && is_optional a then to_mandatory a else a ) args

let fix_tfunc_args args =
	fix_optional_args
		(fun a -> Option.is_some (snd a))
		(fun (v,_) -> (v,None))
		args

let fix_tsignature_args args =
	fix_optional_args
		(fun (_,optional,_) -> optional)
		(fun (name,_,t) -> (name,false,t))
		args

(**
	Inserts `null`s if there are missing optional args before empty rest arguments.
*)
let fix_call_args callee_type exprs =
	match follow callee_type with
	| TFun (args,_) ->
		(match List.rev args with
		| (_,_,t) :: args_rev when is_rest_type t && List.length args_rev > List.length exprs ->
			let rec loop args exprs =
				match args, exprs with
				| [], _ | [_], _ -> exprs
				| (_,_,t) :: args, [] -> (mk (TConst TNull) t null_pos) :: loop args exprs
				| _ :: args, e :: exprs -> e :: loop args exprs
			in
			loop args exprs
		| _ -> exprs
		)
	| _ -> exprs

(**
	Escapes all "$" chars and encloses `str` into double quotes
*)
let quote_string str =
	"\"" ^ (Str.global_replace (Str.regexp "\\$") "\\$" (String.escaped str)) ^ "\""

(**
	Check if specified field is a var with non-constant expression
*)
let is_var_with_nonconstant_expr (field:tclass_field) =
	match field.cf_kind with
		| Var _ ->
			(match field.cf_expr with
				| None -> false
				| Some ({eexpr = TConst _ }) -> false
				| Some _ -> true
			)
		| Method _ -> false
(**
	Check if specified field is an `inline var` field.
*)
let is_inline_var (field:tclass_field) =
	match field.cf_kind with
		| Var { v_read = AccInline; v_write = AccNever } -> true
		| _ -> false

(**
	@return New TBlock expression which is composed of setting default values for optional arguments and function body.
*)
let inject_defaults (ctx:php_generator_context) (func:tfunc) =
	let rec inject args body_exprs =
		match args with
			| [] -> body_exprs
			| (_, None) :: rest -> inject rest body_exprs
			| (_, Some {eexpr = TConst TNull}) :: rest -> inject rest body_exprs
			| (var, Some const) :: rest ->
				let expr = Texpr.set_default ctx.pgc_common.basic var const func.tf_expr.epos in
				expr :: (inject rest body_exprs)
	in
	let exprs =
		match func.tf_expr.eexpr with
			| TBlock exprs -> inject func.tf_args exprs
			| _ -> inject func.tf_args [ func.tf_expr ]
	in
	{
		eexpr = TBlock exprs;
		etype = follow func.tf_expr.etype;
		epos  = func.tf_expr.epos;
	}

(**
	Check if `expr` is a constant string
*)
let is_constant_string expr =
	match expr.eexpr with
		| TConst (TString _) -> true
		| _ -> false

(**
	Check if `expr` is a constant null
*)
let is_constant_null expr =
	match expr.eexpr with
		| TConst TNull -> true
		| _ -> false

(**
	Check if `expr` is a constant
*)
let is_constant expr =
	match expr.eexpr with
		| TConst _ -> true
		| _ -> false

(**
	Check if `expr` is a constant zero
*)
let is_constant_zero expr =
	try
		match expr.eexpr with
			| TConst (TInt i) when i = Int32.zero -> true
			| TConst (TFloat s) when float_of_string s = 0.0 -> true
			| _ -> false
	with _ ->
		false

(**
	Check if `expr` is a concatenation
*)
let is_concatenation expr =
	match expr.eexpr with
		| TBinop (OpAdd, expr1, expr2) -> (is_string expr1) || (is_string expr2)
		| _ -> false

(**
	Check if provided expression is a block of expressions
*)
let is_block expr = match expr.eexpr with TBlock _ -> true | _ -> false

(**
	Check if provided expression is a binary operation
*)
let is_binop expr = match expr.eexpr with TBinop _ -> true | _ -> false

(**
	Check if provided expression is an assignment binary operation
*)
let is_binop_assign expr =
	match expr.eexpr with
		| TBinop ((OpAssign | OpAssignOp _), _, _) -> true
		| _ -> false

(**
	Check if specified expression is field access or array access
*)
let is_access expr =
	match expr.eexpr with
		| TField _ | TArray _ -> true
		| _ -> false

(**
	Check if specified field access is an access to the field `Array.arr`
	It's a private field of the php-specific implementation of Haxe Array.
*)
let is_array_arr faccess =
	match faccess with
		| FInstance ({ cl_path = [],"Array" }, _, { cf_name = "arr" }) -> true
		| _ -> false

(**
	Indicates if `expr` is actually a call to Haxe->PHP magic function
	@see http://old.haxe.org/doc/advanced/magic#php-magic
*)
let is_magic expr =
	match expr.eexpr with
	| TCall ({ eexpr = TIdent name}, _) ->
		(match name with
			| "__php__" -> true
			| "__call__" -> true
			| "__physeq__" -> true
			| "__var__" -> true
			| _ -> false
		)
	| _ -> false

(**
	Check if `expr1` and `expr2` can be reliably checked for equality only with `Boot.equal()`
*)
let need_boot_equal expr1 expr2 =
	if is_constant_null expr1 || is_constant_null expr2 then
		false
	else
		let unknown1 = is_unknown_type expr1.etype
		and unknown2 = is_unknown_type expr2.etype in
		if unknown1 && unknown2 then
			true
		else if is_function_type expr1.etype || is_function_type expr2.etype then
			true
		else
			let int1 = is_int expr1
			and int2 = is_int expr2
			and float1 = is_float expr1
			and float2 = is_float expr2 in
			(int1 && float2)
			|| (float1 && (float2 || int2))
			|| (unknown1 && (int2 || float2))
			|| ((int1 || float1) && unknown2)

(**
	Adds `return` expression to block if it does not have one already
*)
let ensure_return_in_block block_expr =
	match block_expr.eexpr with
		| TBlock [] -> fail block_expr.epos __LOC__
		| TBlock exprs ->
			let reversed = List.rev exprs in
			let last_expr = List.hd reversed in
			let return_expr = { last_expr with eexpr = TReturn (Some last_expr) } in
			let reversed = return_expr::(List.tl reversed) in
			{ block_expr with eexpr = TBlock (List.rev reversed) }
		| _ -> fail block_expr.epos __LOC__

(**
	If `expr` is a block, then return list of expressions in that block.
	Otherwise returns a list with `expr` as a single item.
*)
let unpack_block expr =
		match expr.eexpr with
			| TBlock exprs -> exprs
			| _ -> [ expr ]

(**
	If `expr` is a block of a single expression, then return that single expression.
	If `expr` is a block with multiple expressions, fail compilation.
	Otherwise return `expr` as-is.
*)
let unpack_single_expr_block expr =
		match expr.eexpr with
			| TBlock [ e ] -> e
			| TBlock _ -> fail expr.epos __LOC__
			| _ -> expr

(**
	Check if specified type has rtti meta
*)
let has_rtti_meta ctx mtype =
	match Texpr.build_metadata ctx.basic mtype with
		| None -> false
		| Some _ -> true

(**
	Check if user-defined field has the same name as one of php magic methods, but with not compatible signature.
*)
let field_needs_rename field =
	match field.cf_kind with
		| Var _ -> false
		| Method _ ->
			match field.cf_name with
				| "__construct" | "__destruct" | "__call" | "__callStatic" | "__get" | "__set" | "__isset"
				| "__unset" | "__sleep" | "__wakeup" | "__toString" | "__invoke" | "__set_state" | "__clone"
				| "__debugInfo" -> not (Meta.has Meta.PhpMagic field.cf_meta)
				| _ -> false
(**
	Get valid `field` name.
*)
let field_name field =
	if field_needs_rename field then
		"__hx__renamed" ^ field.cf_name
	else
		field.cf_name

(**
	Check if `expr` is `Std.is`
*)
let is_std_is expr =
	match expr.eexpr with
		| TField (_, FStatic ({ cl_path = path }, { cf_name = ("is" | "isOfType") })) -> path = boot_type_path || path = std_type_path
		| _ -> false

(**
	Check if provided expression is actually a casting to NativeStructArray
*)
let is_native_struct_array_cast expr =
	match expr.eexpr with
		| TCall ({ eexpr = TField (_, field) }, _) ->
			(match field with
				| FStatic ({ cl_path = (["php"; "_NativeStructArray"], "NativeStructArray_Impl_") }, { cf_name = "__fromObject" }) -> true
				| _ -> false
			)
		| _ -> false

(**
	Check if `expr` is an anonymous object declaration
*)
let is_object_declaration expr =
	match (reveal_expr expr).eexpr with
		| TObjectDecl _ -> true
		| _ -> false

(**
	Check if `subject_arg` and `type_arg` can be generated as `$subject instanceof Type` expression.
*)
let instanceof_compatible (subject_arg:texpr) (type_arg:texpr) : bool =
	let is_real_class path =
		match path with
			| ([], "String") | ([], "Class") | (["php";"_NativeArray"], "NativeArray_Impl_") -> false
			| _ -> true
	in
	match (reveal_expr_with_parenthesis type_arg).eexpr with
		| TTypeExpr (TClassDecl { cl_path = path }) when is_real_class path ->
			let subject_arg = reveal_expr_with_parenthesis subject_arg in
			(match subject_arg.eexpr with
				| TLocal _ | TField _ | TCall _ | TArray _ | TConst TThis -> not (is_magic subject_arg)
				| _ -> false
			)
		| _ -> false


(**
	PHP DocBlock types
*)
type doc_type =
	| DocVar of string * (string option) (* (type name, description) *)
	| DocMethod of (string * bool * t) list * t * (string option) (* (arguments, return type, description) *)
	| DocClass of string option

(**
	Common interface for module_type instances
*)
class virtual type_wrapper (type_path:path) (meta:metadata) (needs_generation:bool) =
	object (self)
		(**
			Indicates if this type should be rendered to corresponding php file
		*)
		method needs_generation = needs_generation
		(**
			Indicates if class initialization method should be executed upon class loaded
		*)
		method virtual needs_initialization : bool
		(**
			Returns hx source file name where this type was declared
		*)
		method virtual get_source_file : string
		(**
			Returns `Type.module_type` instance for this type
		*)
		method virtual get_module_type : module_type
		(**
			Returns expression of a user-defined static __init__ method
			@see http://old.haxe.org/doc/advanced/magic#initialization-magic
		*)
		method get_magic_init : texpr option = None
		(**
			Namespace path. E.g. ["some"; "pack"] for "some.pack.MyType"
		*)
		method get_namespace = get_module_path type_path
		(**
			Short type name. E.g. `SomeType` for `pack.SomeType`
		*)
		method get_name = get_type_name type_path
		(**
			Full type path
		*)
		method get_type_path = type_path
		(**
			If current type requires some additional type to be generated
		*)
		method get_service_type : module_type option = None
	end

(**
	TClassDecl
*)
class class_wrapper (cls) =
	object (self)
		inherit type_wrapper cls.cl_path cls.cl_meta (not (has_class_flag cls CExtern))
		(**
			Indicates if class initialization method should be executed upon class loaded
		*)
		method needs_initialization =
			(* Interfaces may need initialization only for RTTI meta data.
				But that meta is written in `class_wrapper#write_rtti_meta` *)
			if (has_class_flag cls CInterface) then
				false
			else
				match cls.cl_init with
					| Some _ -> true
					| None ->
						List.exists
							(fun field ->
								(* Skip `inline var` fields *)
								not (is_inline_var field)
								&& match field.cf_kind, field.cf_expr with
									| Var _, Some { eexpr = TConst (TInt value) } -> value = Int32.min_int
									| Var _, Some { eexpr = TConst _ } -> false
									| Var _, Some _ -> true
									| Method MethDynamic, _ -> true
									| _ -> false
							)
							cls.cl_ordered_statics
		(**
			Returns expression of a user-defined static __init__ method
			@see http://old.haxe.org/doc/advanced/magic#initialization-magic
		*)
		method get_magic_init = cls.cl_init
		(**
			Returns hx source file name where this type was declared
		*)
		method get_source_file = cls.cl_pos.pfile
		(**
			Returns `Type.module_type` instance for this type
		*)
		method get_module_type = TClassDecl cls
		(**
			If current type requires some additional type to be generated
		*)
		method get_service_type : module_type option =
			if not (has_class_flag cls CExtern) then
				None
			else
				match cls.cl_init with
					| None -> None
					| Some body ->
						let path =
							match cls.cl_path with
								| (pack, name) -> (pack, ("_extern_" ^ name))
						in
						let additional_cls = {
							cls with
								cl_path = path;
								cl_fields  = PMap.create (fun a b -> 0);
								cl_statics  = PMap.create (fun a b -> 0);
								cl_ordered_fields  = [];
								cl_ordered_statics  = [];
								cl_constructor = None;
								cl_init = Some body
						} in
						remove_class_flag additional_cls CExtern;
						Some (TClassDecl additional_cls)
	end

(**
	TEnumDecl
*)
class enum_wrapper (enm) =
	object (self)
		inherit type_wrapper enm.e_path enm.e_meta (not enm.e_extern)
		(**
			Indicates if class initialization method should be executed upon class loaded
		*)
		method needs_initialization = false
		(**
			Returns hx source file name where this type was declared
		*)
		method get_source_file = enm.e_pos.pfile
		(**
			Returns `Type.module_type` instance for this type
		*)
		method get_module_type = TEnumDecl enm
	end

(**
	TTypeDecl
*)
class typedef_wrapper (tdef) =
	object (self)
		inherit type_wrapper tdef.t_path tdef.t_meta false
		(**
			Indicates if class initialization method should be executed upon class loaded
		*)
		method needs_initialization = false
		(**
			Returns hx source file name where this type was declared
		*)
		method get_source_file = tdef.t_pos.pfile
		(**
			Returns `Type.module_type` instance for this type
		*)
		method get_module_type = TTypeDecl tdef
	end

(**
	TAbstractDecl
*)
class abstract_wrapper (abstr) =
	object (self)
		inherit type_wrapper abstr.a_path abstr.a_meta false
		(**
			Indicates if class initialization method should be executed upon class loaded
		*)
		method needs_initialization = false
		(**
			Returns hx source file name where this type was declared
		*)
		method get_source_file = abstr.a_pos.pfile
		(**
			Returns `Type.module_type` instance for this type
		*)
		method get_module_type = TAbstractDecl abstr
	end

(**
	type_wrapper from table
*)
let get_stored_wrapper tbl wrap key : type_wrapper =
	try
		let wrapper = Hashtbl.find tbl key in
		wrapper
	with Not_found ->
		let wrapper = wrap key in
		Hashtbl.add tbl key wrapper;
		wrapper

(**
	type_wrapper for classes
*)
let classes = Hashtbl.create 1000
let get_class_wrapper = get_stored_wrapper classes (fun cls -> new class_wrapper cls)

(**
	type_wrapper for enums
*)
let enums = Hashtbl.create 200
let get_enum_wrapper = get_stored_wrapper enums (fun enm -> new enum_wrapper enm)

(**
	type_wrapper for typedefs
*)
let typedefs = Hashtbl.create 200
let get_typedef_wrapper = get_stored_wrapper typedefs (fun typedef -> new typedef_wrapper typedef)

(**
	type_wrapper for abstracts
*)
let abstracts = Hashtbl.create 200
let get_abstract_wrapper = get_stored_wrapper abstracts (fun abstr -> new abstract_wrapper abstr)

(**
	Returns wrapper for module_type.
	Caches wrappers so that each type will always return the same wrapper instance.
*)
let get_wrapper (mtype:module_type) : type_wrapper =
	match mtype with
		| TClassDecl cls -> get_class_wrapper cls
		| TEnumDecl enm -> get_enum_wrapper enm
		| TTypeDecl typedef -> get_typedef_wrapper typedef
		| TAbstractDecl abstr -> get_abstract_wrapper abstr

(**
	Drop cached instances of type_wrapper
*)
let clear_wrappers () =
	Hashtbl.clear classes;
	Hashtbl.clear enums;
	Hashtbl.clear typedefs;
	Hashtbl.clear abstracts

(**
	Check if specified type name is used in specified namespace
*)
let type_name_used_in_namespace ctx type_path as_name namespace =
	let types =
		match Hashtbl.find_all ctx.pgc_namespaces_types_cache namespace with
			| [] ->
				List.iter
					(fun ctx_type ->
						let wrapper = get_wrapper ctx_type in
						Hashtbl.add ctx.pgc_namespaces_types_cache wrapper#get_namespace (StringHelper.uppercase wrapper#get_name)
					)
					ctx.pgc_common.types;
				Hashtbl.find_all ctx.pgc_namespaces_types_cache namespace
			| types -> types
	in
	List.mem (StringHelper.uppercase as_name) types
	&& (namespace, as_name) <> type_path

(**
	Class to simplify collecting lists of declared and used local vars.
	Collected data is needed to generate closures correctly.
*)
class local_vars =
	object (self)
		(** Hashtbl to collect local var used in current scope *)
		val mutable used_locals = [Hashtbl.create 100]
		(** Hashtbl to collect local vars declared in current scope *)
		val mutable declared_locals = [Hashtbl.create 100]
		(** Local vars which were captured in closures (passed via `use` directive in php) *)
		val captured_locals = Hashtbl.create 0
		(**
			Clear collected data
		*)
		method clear : unit =
			used_locals <- [Hashtbl.create 100];
			declared_locals <- [Hashtbl.create 100];
			Hashtbl.clear captured_locals
		(**
			This method should be called upone entering deeper scope.
			E.g. right before processing a closure. Just before closure arguments handling.
		*)
		method dive : unit =
			used_locals <- (Hashtbl.create 100) :: used_locals;
			declared_locals <- (Hashtbl.create 100) :: declared_locals
		(**
			This method should be called right after leaving a scope.
			@return List of vars names used in finished scope, but declared in higher scopes.
					And list of vars names declared in finished scope.
					And list of vars names declared in finished scope and captured by closures via `use` directive
		*)
		method pop : string list * string list * string list =
			match used_locals with
				| [] -> die "" __LOC__
				| used :: rest_used ->
					match declared_locals with
						| [] -> die "" __LOC__
						| declared :: rest_declared ->
							let higher_vars = diff_lists (hashtbl_keys used) (hashtbl_keys declared)
							and declared_vars = hashtbl_keys declared in
							used_locals <- rest_used;
							declared_locals <- rest_declared;
							List.iter self#used higher_vars;
							let captured_vars = intersect_lists declared_vars (hashtbl_keys captured_locals) in
							List.iter (fun name -> Hashtbl.remove captured_locals name) declared_vars;
							(higher_vars, declared_vars, captured_vars)
		(**
			This method should be called right after leaving a scope.
			@return List of vars names used in finished scope, but declared in higher scopes
		*)
		method pop_used : string list = match self#pop with (higher_vars, _, _) -> higher_vars
		(**
			This method should be called right after leaving a scope.
			@return List of vars names declared in finished scope
		*)
		method pop_declared : string list = match self#pop with (_, declared_vars, _) -> declared_vars
		(**
			Get current list of captured variables.
			After leaving a scope all vars declared in that scope get removed from a list of captured variables.
		*)
		method pop_captured : string list = match self#pop with (_, _, captured_vars) -> captured_vars
		(**
			Specify local var name declared in current scope
		*)
		method declared (name:string) : unit =
			match declared_locals with
				| [] -> die "" __LOC__
				| current :: _ -> Hashtbl.replace current name name
		(**
			Specify local var name used in current scope
		*)
		method used (name:string) : unit =
			match used_locals with
				| [] -> die "" __LOC__
				| current :: _ -> Hashtbl.replace current name name
		(**
			Mark specified vars as captured by closures.
		*)
		method captured (var_names:string list) : unit =
			List.iter (fun name -> Hashtbl.replace captured_locals name name) var_names
	end

(**
	Consumes expressions and generates php code to output buffer.
*)
class code_writer (ctx:php_generator_context) hx_type_path php_name =
	object (self)
		(** Namespace path. E.g. ["some"; "pack"] for "some.pack.MyType" *)
		val namespace = get_module_path hx_type_path
		(** List of types for "use" section *)
		val use_table = Hashtbl.create 50
		(** Output buffer *)
		val mutable buffer = Buffer.create 1024
		(** Intendation used for each line written *)
		val mutable indentation = ""
		(** Expressions nesting. E.g. "if(callFn(ident))" will be represented as [ident, callFn, if] *)
		val mutable expr_hierarchy : texpr list = []
		(** Object to collect local vars declarations and usage as we iterate through methods' expressions *)
		val vars = new local_vars
		(** Sourcemap generator *)
		val mutable sourcemap : sourcemap_builder option = None
		(** Indicates if `super()` expressions should be generated if spotted. *)
		val mutable has_super_constructor = true
		(** The latest string written to the output buffer via `self#write_pos` method *)
		val mutable last_written_pos = ""
		(**
			Get php name of current type
		*)
		method get_name : string = php_name
		(**
			Returns generated file contents
		*)
		method get_contents = Buffer.contents buffer
		(**
			Clears current generated content
		*)
		method clear_contents = Buffer.clear buffer
		(**
			Reset current state (expr hierarchy, indentation, local vars)
		*)
		method reset =
			vars#clear;
			self#indent 0;
			expr_hierarchy <- []
		(**
			Set sourcemap generator
		*)
		method set_sourcemap_generator generator = sourcemap <- Some generator
		(**
			Get sourcemap generator
		*)
		method get_sourcemap_generator = sourcemap
		(**
			Make this writer skip generation of `super()` expression if spotted.
		*)
		method extends_no_constructor = has_super_constructor <- false
		(**
			Increase indentation by one level
		*)
		method indent_more =
			indentation <- indentation ^ "\t";
		(**
			Decrease indentation by one level
		*)
		method indent_less =
			indentation <- String.make ((String.length indentation) - 1) '\t';
		(**
			Set indentation level (starting from zero for no indentation)
		*)
		method indent level =
			indentation <- String.make level '\t';
		(**
			Get indentation level (starting from zero for no indentation)
		*)
		method get_indentation = String.length indentation
		(**
			Set indentation level (starting from zero for no indentation)
		*)
		method set_indentation level =
			indentation <- String.make level '\t'
		(**
			Specify local var name declared in current scope
		*)
		method declared_local_var name = vars#declared name
		(**
			Adds type to "use" section if not added yet.
			If it's a top-level type then type name returned without adding to "use" section.
			@return Unique alias for specified type.
		*)
		method use ?prefix (type_path:path) =
			if type_path = hx_type_path then
				php_name
			else if get_type_name type_path = "" then
				match get_module_path type_path with
				| [] -> "\\"
				| module_path -> "\\" ^ (String.concat "\\" (get_real_path module_path)) ^ "\\"
			else begin
				let orig_type_path = type_path in
				let type_path = match type_path with (pack, name) -> (pack, get_real_name name) in
				let type_path =
					match prefix with
						| Some false -> type_path
						| _ -> add_php_prefix ctx type_path
				in
				let module_path = get_module_path type_path in
				match type_path with
					| ([], type_name) -> "\\" ^ type_name
					| _ ->
						let alias_source = ref (List.rev module_path) in
						let get_alias_next_part () =
							match !alias_source with
								| [] ->  fail ~msg:("Failed to find already used type: " ^ get_full_type_name type_path) self#pos __LOC__
								| name :: rest ->
									alias_source := (match rest with
										| [] -> [name]
										| _ -> rest
									);
									StringHelper.capitalize name
						and added = ref false
						and alias = ref (get_type_name type_path) in
						let alias_upper = ref (StringHelper.uppercase !alias) in
						let prepend_alias prefix =
							alias := prefix ^ !alias;
							alias_upper := StringHelper.uppercase !alias
						in
						if !alias_upper = (StringHelper.uppercase php_name) then
							prepend_alias (get_alias_next_part ());
						while not !added do
							try
								if (get_module_path type_path) <> namespace && type_name_used_in_namespace ctx orig_type_path !alias namespace then
									prepend_alias (get_alias_next_part ())
								else
									let used_type = Hashtbl.find use_table !alias_upper in
									if used_type.ut_type_path = type_path then
										added := true
									else
										prepend_alias (get_alias_next_part ());
							with
								| Not_found ->
									Hashtbl.add use_table !alias_upper { ut_alias = !alias; ut_type_path = type_path; };
									added := true
								| _ -> fail self#pos __LOC__
						done;
						!alias
			end
		(**
			Extracts type path from Type.t value and execute self#use on it
			@return Unique alias for specified type.
		*)
		method use_t ?(for_doc=false) (t_inst:Type.t) =
			match follow t_inst with
				| TEnum (tenum, _) -> self#use tenum.e_path
				| TInst (tcls, params) ->
					(match tcls.cl_kind with
						| KTypeParameter _ -> "mixed"
						| _ ->
							(match tcls.cl_path, params with
								| ([], "String"), _ -> "string"
								| ([], "Array"), [param] when for_doc -> (self#use_t param) ^ "[]|" ^ (self#use tcls.cl_path)
								| _ -> self#use ~prefix:(not (has_class_flag tcls CExtern)) tcls.cl_path
							)
					)
				| TFun _ -> self#use ~prefix:false ([], "Closure")
				| TAnon _ -> "object"
				| TDynamic _ -> "mixed"
				| TLazy _ -> fail ~msg:"TLazy not implemented" self#pos __LOC__
				| TMono mono ->
					(match mono.tm_type with
						| None -> "mixed"
						| Some t -> self#use_t t
					)
				| TType _ -> fail ~msg:"TType not implemented" self#pos __LOC__
				| TAbstract (abstr, _) ->
					match abstr.a_path with
						| ([],"Int") -> "int"
						| ([],"Float") -> "float"
						| ([],"Bool") -> "bool"
						| ([],"Void") -> "void"
						| ([],"Enum") -> "Enum"
						| ([],"Class") -> "Class"
						| (["php"],"NativeArray") when for_doc ->
							(match Type.follow t_inst with
								| TAbstract ({ a_path = ["php"],"NativeIndexedArray" }, [param]) -> (self#use_t param) ^ "[]"
								| _ -> "array"
							)
						| (["php"],"NativeArray") -> "array"
						| _ when Meta.has Meta.CoreType abstr.a_meta -> "mixed"
						| _ -> self#use_t abstr.a_this
		(**
			Position of currently generated code in source hx files
		*)
		method pos =
			match expr_hierarchy with
				| { epos = pos } :: _ -> pos
				| _ -> null_pos
		(**
			Indicates whether current expression nesting level is a top level of a block
		*)
		method parent_expr_is_block single_expr_is_not_block =
			let rec expr_is_block expr parents no_parent_is_block =
				match expr.eexpr with
					| TBlock [_] when single_expr_is_not_block ->
						(match parents with
							| { eexpr = TBlock _ } :: _ -> true
							| { eexpr = TFunction _ } :: _ -> true
							| _ :: _ -> false
							| [] -> no_parent_is_block
						)
					| TBlock _ -> true
					| TIf (_, if_expr, Some else_expr) ->
						if (expr_is_block if_expr [] false) || (expr_is_block else_expr [] false) then
							true
						else
							(match parents with
								| parent :: rest -> expr_is_block parent rest true
								| [] -> false
							)
					| TIf (_, _, None) -> true
					| TTry _ -> true
					| TWhile _ -> true
					| TFor _ -> true
					| TSwitch _ -> true
					| _ -> false
			in
			match expr_hierarchy with
				| _ :: parent :: rest -> expr_is_block parent rest true
				| _ -> false
		(**
			Returns parent expression  (bypasses casts and metas)
		*)
		method parent_expr =
			let rec traverse expr parents =
				match expr.eexpr with
					| TCast (_, None)
					| TMeta _ ->
						(match parents with
							| parent :: rest -> traverse parent rest
							| [] -> None
						)
					| _ -> Some expr
			in
			match expr_hierarchy with
				| _ :: parent :: rest -> traverse parent rest
				| _ -> None
		(**
			Indicates if parent expression is a call (bypasses casts and metas)
		*)
		method parent_expr_is_call =
			match self#parent_expr with
				| Some { eexpr = TCall _ } -> true
				| _ -> false
		(**
			Indicates if current expression is passed to `php.Ref<T>`
		*)
		method current_expr_is_for_ref =
			match expr_hierarchy with
				| [] -> false
				| current :: _ ->
					match self#parent_expr with
						| Some { eexpr = TCall (target, params) } when current != (reveal_expr target) ->
							(match follow target.etype with
								| TFun (args,_) ->
									let rec check args params =
										match args, params with
										| (_, _, t) :: _, param :: _ when current == (reveal_expr param) ->
											is_ref t
										| _, [] | [], _ ->
											false
										| _ :: args, _ :: params ->
											check args params
									in
									check args params
								| _ -> false
							)
						| _ -> false
		(**
			Check if currently generated expression is located in a left part of assignment.
		*)
		method is_in_write_context =
			let is_in_ref_arg current types args =
				try List.exists2 (fun (_,_,t) arg -> arg == current && is_ref t) types args
				with Invalid_argument _ -> false
			in
			let rec traverse current parents =
				match parents with
					| { eexpr = TBinop(OpAssign, left_expr, _) } :: _
					| { eexpr = TBinop(OpAssignOp _, left_expr, _) } :: _ -> left_expr == current
					| { eexpr = TUnop(op, _, _) } :: _ -> is_modifying_unop op
					| { eexpr = TCall({ etype = TFun(types,_) }, args) } :: _ when is_in_ref_arg current types args -> true
					| [] -> false
					| parent :: rest -> traverse parent rest
			in
			match expr_hierarchy with
				| current :: parents -> traverse current parents
				| _ -> false
		(**
			Add a function call to "dereference" part of expression to avoid "Cannot use temporary expression in write context"
			erro in expressions like:
			```
			new MyClass().fieldName = 'value';
			```
		*)
		method dereference expr =
			let deref expr =
				{ expr with eexpr = TCall (
					{ expr with eexpr = TField (
						{ expr with eexpr = TTypeExpr (TClassDecl ctx.pgc_boot) },
						FStatic (ctx.pgc_boot, PMap.find "deref" ctx.pgc_boot.cl_statics)
					) },
					[ expr ]
				) }
			in
			match expr.eexpr with
				| TField (target_expr, access) ->
					{
						expr with eexpr = TField (deref target_expr, access)
					}
				| TArray (target_expr, access_expr) ->
					{
						expr with eexpr = TArray (deref target_expr, access_expr)
					}
				| _ -> fail self#pos __LOC__
		(**
			Writes specified string to output buffer
		*)
		method write str =
			Buffer.add_string buffer str;
			Option.may (fun smap -> smap#insert (SMStr str)) sourcemap;
		(**
			Writes specified string to output buffer without affecting sourcemap generator
		*)
		method write_bypassing_sourcemap str =
			Buffer.add_string buffer str;
		(**
			Writes constant double-quoted string to output buffer
		*)
		method write_const_string str =
			self#write ("\"" ^ (escape_bin str) ^ "\"")
		(**
			Writes fixed amount of empty lines (E.g. between methods)
		*)
		method write_empty_lines =
			self#write "\n"
		(**
			Writes current indentation to output buffer
		*)
		method write_indentation =
			self#write indentation
		(**
			Writes current indentation followed by `str` to output buffer
		*)
		method write_with_indentation str =
			self#write indentation;
			self#write str
		(**
			Writes specified line to output buffer and appends \n
		*)
		method write_line line =
			self#write (indentation ^ line ^ "\n")
		(**
			Writes specified statement to output buffer and appends ";\n"
		*)
		method write_statement statement =
			self#write (indentation ^ statement ^ ";\n")
		(**
			Build "use" statements
		*)
		method write_use =
			self#indent 0;
			let write _ used_type =
				let namespace =
					if hx_type_path = ([],"") then namespace (* ([],"") is for index.php *)
					else ctx.pgc_prefix @ namespace
				in
				if (get_module_path used_type.ut_type_path) <> namespace then
					if get_type_name used_type.ut_type_path = used_type.ut_alias then
						self#write_statement ("use " ^ (get_full_type_name used_type.ut_type_path))
					else
						let full_name = get_full_type_name used_type.ut_type_path in
						self#write_statement ("use " ^ full_name ^ " as " ^ used_type.ut_alias)
			in
			Hashtbl.iter write use_table
		(**
			Writes array item declaration to output buffer and appends ",\n"
			Adds indentation and ",\n" if `separate_line` is `true`.
		*)
		method write_array_item ?separate_line ?key value_expr =
			let separate_line = match separate_line with Some true -> true | _ -> false in
			if separate_line then self#write_indentation;
			(match key with
				| None ->
					self#write_expr value_expr;
				| Some key_str ->
					self#write ((quote_string key_str) ^ " => ");
					self#write_expr value_expr
			);
			if separate_line then self#write ",\n"
		(**
			Writes expression to output buffer
		*)
		method write_expr (expr:texpr) =
			expr_hierarchy <- expr :: expr_hierarchy;
			Option.may (fun smap -> smap#insert (SMPos expr.epos)) sourcemap;
			(match expr.eexpr with
				| TConst const -> self#write_expr_const const
				| TLocal var ->
					vars#used (vname var.v_name);
					self#write ("$" ^ (vname var.v_name))
				| TArray (target, index) -> self#write_expr_array_access target index
				| TBinop (OpAssign, { eexpr = TArray (target, index) }, value) when is_array_type target.etype ->
					self#write_expr_set_array_item target index value
				| TBinop (operation, expr1, expr2) when needs_dereferencing (is_assignment_binop operation) expr1 ->
					self#write_expr { expr with eexpr = TBinop (operation, self#dereference expr1, expr2) }
				| TBinop (operation, expr1, expr2) -> self#write_expr_binop operation expr1 expr2
				| TField ({ eexpr = TArrayDecl exprs }, faccess) when is_array_arr faccess && not self#current_expr_is_for_ref ->
					self#write_native_array_decl exprs
				| TField (fexpr, access) when is_php_global expr -> self#write_expr_php_global expr
				| TField (fexpr, access) when is_php_class_const expr -> self#write_expr_php_class_const expr
				| TField (fexpr, access) when needs_dereferencing (self#is_in_write_context) expr -> self#write_expr (self#dereference expr)
				| TField (fexpr, access) -> self#write_expr_field fexpr access
				| TTypeExpr mtype -> self#write_expr_type mtype
				| TParenthesis expr ->
					self#write "(";
					self#write_expr expr;
					self#write ")"
				| TObjectDecl fields -> self#write_expr_object_declaration fields
				| TArrayDecl exprs -> self#write_expr_array_decl exprs
				| TCall (target, [arg1; arg2]) when is_std_is target -> self#write_expr_std_is target arg1 arg2
				| TCall (_, [arg]) when is_native_struct_array_cast expr && is_object_declaration arg ->
					(match (reveal_expr arg).eexpr with TObjectDecl fields -> self#write_assoc_array_decl fields | _ -> fail self#pos __LOC__)
				| TCall ({ eexpr = TIdent name}, args) when is_magic expr -> self#write_expr_magic name args
				| TCall ({ eexpr = TField (expr, access) }, args) when is_string expr -> self#write_expr_call_string expr access args
				| TCall (expr, args) when is_syntax_extern expr -> self#write_expr_call_syntax_extern expr args
				| TCall (target, args) -> self#write_expr_call target args
				| TNew (_, _, args) when is_string expr -> write_args self#write self#write_expr args
				| TNew (tcls, _, args) -> self#write_expr_new tcls args
				| TUnop (operation, flag, target_expr) when needs_dereferencing (is_modifying_unop operation) target_expr ->
					self#write_expr { expr with eexpr = TUnop (operation, flag, self#dereference target_expr) }
				| TUnop (operation, flag, expr) -> self#write_expr_unop operation flag expr
				| TFunction fn -> self#write_expr_function fn
				| TVar (var, expr) -> self#write_expr_var var expr
				| TBlock exprs -> self#write_expr_block expr
				| TFor (var, iterator, body) -> fail self#pos __LOC__
				| TIf (condition, if_expr, else_expr) -> self#write_expr_if condition if_expr else_expr
				| TWhile (condition, expr, do_while) ->
					(match (reveal_expr_with_parenthesis condition).eexpr with
						| TField (_, FStatic ({ cl_path = path }, { cf_name = "foreachCondition" })) when path = syntax_type_path  ->
							self#write_expr_syntax_foreach expr
						| _ ->
							self#write_expr_while condition expr do_while
					)
				| TSwitch (switch, cases, default ) -> self#write_expr_switch switch cases default
				| TTry (try_expr, catches) -> self#write_expr_try_catch try_expr catches
				| TReturn expr -> self#write_expr_return expr
				| TBreak -> self#write "break"
				| TContinue -> self#write "continue"
				| TThrow expr -> self#write_expr_throw expr
				| TCast (expr, mtype) -> self#write_expr_cast expr mtype
				| TMeta (_, expr) -> self#write_expr expr
				| TEnumParameter (expr, constructor, index) -> self#write_expr_enum_parameter expr constructor index
				| TEnumIndex expr -> self#write_expr_enum_index expr
				| TIdent s -> self#write s
			);
			expr_hierarchy <- List.tl expr_hierarchy
		(**
			Writes TConst to output buffer
		*)
		method write_expr_const const =
			match const with
				| TFloat str -> self#write str
				| TString str -> self#write_const_string str
				| TBool value -> self#write (if value then "true" else "false")
				| TNull -> self#write "null"
				| TThis -> self#write "$this"
				| TSuper -> self#write "parent"
				| TInt value ->
					(* See https://github.com/HaxeFoundation/haxe/issues/5289 *)
					if value = Int32.min_int then
						self#write "((int)-2147483648)"
					else
						self#write (Int32.to_string value)
		(**
			Writes TArrayDecl to output buffer
		*)
		method write_expr_array_decl exprs =
			match exprs with
				| [] ->
					let decl () = self#write ("new " ^ (self#use array_type_path) ^ "()") in
					(* Wrap into parentheses if trying to access items of empty array declaration *)
					(match self#parent_expr with
						| Some { eexpr = TArray _ } ->
							self#write "(";
							decl();
							self#write ")"
						| _ ->
							decl()
					)
				| _ ->
					self#write ((self#use array_type_path) ^ "::wrap(");
					self#write_native_array_decl exprs;
					self#write ")"
		(**
			Writes native array declaration to output buffer
		*)
		method write_native_array_decl exprs =
			match exprs with
				| [] ->
					self#write "[]";
				| [expr] ->
					self#write "[";
					self#write_expr expr;
					self#write "]"
				| _ ->
					self#write "[\n";
					self#indent_more;
					List.iter (fun expr -> self#write_array_item ~separate_line:true expr) exprs;
					self#indent_less;
					self#write_with_indentation "]"
		(**
			Write associative array declaration
		*)
		method write_assoc_array_decl fields =
			match fields with
				| [] -> self#write "[]"
				| [((key, _, _), value)] ->
					self#write "[";
					self#write_array_item ~key:key value;
					self#write "]"
				| _ ->
					self#write "[\n";
					self#indent_more;
					let write_field ((key,_,_), value) = self#write_array_item ~separate_line:true ~key:key value in
					List.iter write_field fields;
					self#indent_less;
					self#write_with_indentation "]"
		(**
			Writes `target[index] = value` assuming `target` is of `Array` type
		*)
		method write_expr_set_array_item target index value =
			self#write_expr target;
			self#write "->offsetSet(";
			write_args self#write self#write_expr [index; value];
			self#write ")"
		(**
			Writes TArray to output buffer
		*)
		method write_expr_array_access target index =
			let write_index left_bracket right_bracket =
				self#write left_bracket;
				self#write_expr index;
				self#write right_bracket
			in
			let write_fast_access () =
				self#write "(";
				self#write_expr target;
				self#write "->arr";
				write_index "[" "] ?? null)"
			and write_normal_access () =
				self#write_expr target;
				write_index "[" "]"
			in
			match follow target.etype with
				| TInst ({ cl_path = path }, _) when path = array_type_path ->
					if self#is_in_write_context then
						write_normal_access()
					else
						write_fast_access()
				| _ ->
					write_normal_access ()
		(**
			Writes TVar to output buffer
		*)
		method write_expr_var var expr =
			vars#declared (vname var.v_name);
			self#write ("$" ^ (vname var.v_name) ^ " = ");
			match expr with
				| None -> self#write "null"
				| Some expr -> self#write_expr expr
		(**
			Writes TFunction to output buffer
		*)
		method write_expr_function func =
			self#write_closure_declaration func self#write_function_arg
		(**
			Writes closure declaration to output buffer
		*)
		method write_closure_declaration func write_arg =
			vars#dive;
			self#write "function (";
			write_args self#write write_arg (fix_tfunc_args func.tf_args);
			self#write ")";
			(* Generate closure body to separate buffer *)
			let original_buffer = buffer in
			let sm_pointer_before_body = get_sourcemap_pointer sourcemap in
			buffer <- Buffer.create 256;
			self#write_expr (inject_defaults ctx func);
			let body = Buffer.contents buffer in
			buffer <- original_buffer;
			set_sourcemap_pointer sourcemap sm_pointer_before_body;
			(* Capture local vars used in closures *)
			let used_vars = vars#pop_used in
			vars#captured used_vars;
			self#write " ";
			if List.length used_vars > 0 then begin
				self#write "use (";
				write_args self#write (fun name -> self#write ("&$" ^ name)) used_vars;
				self#write ") "
			end;
			self#write_bypassing_sourcemap body;
			Option.may (fun smap -> smap#fast_forward) sourcemap
		(**
			Writes TBlock to output buffer
		*)
		method write_expr_block block_expr =
			(* Check if parent expr could not contain blocks in PHP, and this block needs to be wrapped in a closure. *)
			let needs_closure = match self#parent_expr with
				| None -> false
				| Some e ->
					match e.eexpr with
						| TIf (_, _, _) -> false
						| TWhile (_, _, _) -> false
						| TTry (_, _) -> false
						| TFor (_, _, _) -> false
						| TFunction _ -> false
						| TBlock _ -> false
						| TSwitch (_, _, _) -> false
						| _ -> true
			in
			if needs_closure then
				begin
					self#write "(";
					self#write_expr {
						block_expr with eexpr = TFunction {
							tf_args = [];
							tf_type = block_expr.etype;
							tf_expr = ensure_return_in_block block_expr;
						}
					};
					self#write ")()"
				end
			else
				begin
					let inline_block = self#parent_expr_is_block false in
					self#write_as_block ~inline:inline_block block_expr
				end
		(**
			Emulates TBlock for parent expression and writes `expr` as inlined block
		*)
		method write_fake_block expr =
			match expr.eexpr with
				| TBlock [] -> ()
				| _ ->
					self#write_indentation;
					let fake_block = { expr with eexpr = TBlock [expr] } in
					expr_hierarchy <- fake_block :: expr_hierarchy;
					self#write_as_block ~inline:true expr;
					expr_hierarchy <- List.tl expr_hierarchy
		(**
			Write position of specified expression to output buffer
		*)
		method write_pos expr =
			let pos = ("#" ^ (stringify_pos expr.epos) ^ "\n") in
			if pos = last_written_pos then
				false
			else begin
				last_written_pos <- pos;
				self#write pos;
				true
			end
		(**
			Writes "{ <expressions> }" to output buffer
		*)
		method write_as_block ?inline ?unset_locals expr =
			let unset_locals = match unset_locals with Some true -> true | _ -> false
			and exprs = match expr.eexpr with TBlock exprs -> exprs | _ -> [expr] in
			let write_body () =
				let write_expr expr =
					if not ctx.pgc_skip_line_directives && not (is_block expr) && expr.epos <> null_pos then
						if self#write_pos expr then self#write_indentation;
					match expr.eexpr with
						| TBlock _ ->
							self#write_as_block ~inline:true expr
						| _ ->
							self#write_expr expr;
							match expr.eexpr with
								| TBlock _ | TIf _ | TTry _ | TSwitch _ | TWhile (_, _, NormalWhile) -> self#write "\n"
								| _ -> self#write ";\n"
				in
				let write_expr_with_indent expr =
					self#write_indentation;
					write_expr expr
				in
				let write_exprs () =
					match exprs with
						| [] -> ()
						| first :: rest ->
							write_expr first; (* write first expression without indentation in case of block inlining *)
							List.iter write_expr_with_indent rest
				in
				if unset_locals then
					begin
						let original_buffer = buffer in
						let sm_pointer_before_body = get_sourcemap_pointer sourcemap in
						buffer <- Buffer.create 256;
						vars#dive;
						write_exprs();
						let body = Buffer.contents buffer in
						buffer <- original_buffer;
						set_sourcemap_pointer sourcemap sm_pointer_before_body;
						let locals = vars#pop_captured in
						if List.length locals > 0 then begin
							self#write ("unset($" ^ (String.concat ", $" locals) ^ ");\n");
							self#write_indentation
						end;
						self#write_bypassing_sourcemap body;
						Option.may (fun smap -> smap#fast_forward) sourcemap
					end
				else
					write_exprs()
			in
			match inline with
				| Some true -> write_body ()
				| _ ->
					self#write "{\n";
					self#indent_more;
					(match exprs with
						| [] -> ()
						| _ ->
							self#write_indentation; (* indentation for the first expression in block *)
							write_body ()
					);
					self#indent_less;
					self#write_with_indentation "}"
		(**
			Writes TReturn to output buffer
		*)
		method write_expr_return expr =
			match expr with
				| None -> self#write "return";
				| Some expr ->
					self#write "return ";
					self#write_expr expr
		(**
			Writes TThrow to output buffer
		*)
		method write_expr_throw expr =
			self#write "throw ";
			self#write_expr expr
		(**
			Writes try...catch to output buffer
		*)
		method write_expr_try_catch try_expr catches =
			self#write "try ";
			self#write_as_block try_expr;
			let rec traverse = function
				| [] -> ()
				| (v,body) :: rest ->
					self#write (" catch(" ^ (self#use_t v.v_type) ^ " $" ^ (vname v.v_name) ^ ") ");
					vars#declared (vname v.v_name);
					self#write_as_block body;
					traverse rest
			in
			traverse catches
		(**
			Writes TCast to output buffer
		*)
		method write_expr_cast expr (mtype:module_type option) =
			match mtype with
			