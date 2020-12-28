open Json

exception Prebuild_error of string

type parsed_warning = {
	w_name : string;
	w_doc : string;
	w_parent : string option;
	w_generic : bool;
}

let as_string = function
	| JString s -> Some s
	| _ -> None

let as_int = function
	| JInt i -> Some i
	| _ -> None

let as_params = function
	| JArray s -> Some (List.map (function
			| JString s -> s
			| _ -> raise (Prebuild_error "parameter description should be a string")
		) s)
	| _ -> None

let as_platforms = function
	| JArray s -> Some (List.map (function
			| JString "cross" -> "Cross"
			| JString "js" -> "Js"
			| JString "lua" -> "Lua"
			| JString "neko" -> "Neko"
			| JString "flash" -> "Flash"
			| JString "php" -> "Php"
			| JString "cpp" -> "Cpp"
			| JString "cs" -> "Cs"
			| JString "java" -> "Java"
			| JString "python" -> "Python"
			| JString "hl" -> "Hl"
			| JString "eval" -> "Eval"
			| _ -> raise (Prebuild_error "invalid platform")
		) s)
	| _ -> None

let as_targets = function
	| JArray s -> Some (List.map (function
			| JString "TClass" -> "TClass"
			| JString "TClassField" -> "TClassField"
			| JString "TAbstract" -> "TAbstract"
			| JString "TAbstractField" -> "TAbstractField"
			| JString "TEnum" -> "TEnum"
			| JString "TTypedef" -> "TTypedef"
			| JString "TAnyField" -> "TAnyField"
			| JString "TExpr" -> "TExpr"
			| JString "TTypeParameter" -> "TTypeParameter"
			| _ -> raise (Prebuild_error "invalid metadata target")
		) s)
	| _ -> None

let as_bool = function
	| JBool b -> Some b
	| _ -> None

let as_links = function
	| JArray s -> Some (List.map (function
			| JString s -> s
			| _ -> raise (Prebuild_error "link should be a string")
		) s)
	| _ -> None

let get_optional_field name map default fields =
	try
		let field = List.find (fun (n, _) -> n = name) fields in
		let value = map (snd field) in
		match value with
		| None -> raise (Prebuild_error ("field `" ^ name ^ "` has invalid data"))
		| Some v -> v
	with Not_found -> default

let get_optional_field2 name map fields =
	try
		let field = List.find (fun (n, _) -> n = name) fields in
		let value = map (snd field) in
		match value with
		| None -> raise (Prebuild_error ("field `" ^ name ^ "` has invalid data"))
		| Some v -> Some v
	with Not_found ->
		None

let get_field name map fields =
	let field = try List.find (fun (n, _) -> n = name) fields with Not_found -> raise (Prebuild_error ("no `" ^ name ^ "` field")) in
	let value = map (snd field) in
	match value with
	| None -> raise (Prebuild_error ("field `" ^ name ^ "` has invalid data"))
	| Some v -> v

let parse_define json =
	let fields = match json with
		| JObject fl -> fl
		| _ -> raise (Prebuild_error "not an object")
	in
	(* name *) get_field "name" as_string fields,
	(* define *) get_field "define" as_string fields,
	(* doc *) get_field "doc" as_string fields,
	(* params *) get_optional_field "params" as_params [] fields,
	(* platforms *) get_optional_field "platforms" as_platforms [] fields,
	(* links *) get_optional_field "links" as_links [] fields

let parse_meta json =
	let fields = match json with
		| JObject fl -> fl
		| _ -> raise (Prebuild_error "not an object")
	in
	(* name *) get_field "name" as_string fields,
	(* metadata *) get_field "metadata" as_string fields,
	(* doc *) get_field "doc" as_string fields,
	(* params *) get_optional_field "params" as_params [] fields,
	(* platforms *) get_optional_field "platforms" as_platforms [] fields,
	(* targets *) get_optional_field "targets" as_targets [] fields,
	(* internal *) get_optional_fiel