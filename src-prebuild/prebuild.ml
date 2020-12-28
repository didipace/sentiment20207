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
			| _ -> raise (Prebuild_error "parameter description should be a