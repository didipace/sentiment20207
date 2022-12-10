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
let get_visibility (meta:metadata) = if Meta.has Meta.Protected meta then "protected" else 