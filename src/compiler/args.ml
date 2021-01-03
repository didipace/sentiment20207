open Globals
open Common
open CompilationContext

let limit_string s offset =
	let rest = 80 - offset in
	let words = ExtString.String.nsplit s " " in
	let rec loop i words = match words with
		| word :: words ->
			if String.length word + i + 1 > rest then (Printf.sprintf "\n%*s" offset "") :: word :: loop (String.length word) words
			else (if i = 0 then "" else " ") :: word :: loop (i + 1 + String.length word) words
		| [] ->
			[]
	in
	String.concat "" (loop 0 words)

let usage_string ?(print_cat=true) arg_spec usage =
	let make_label = fun names hint -> Printf.sprintf "%s %s" (String.concat ", " names) hint in
	let args = (List.filter (fun (cat, ok, dep, spec, hint, doc) -> (List.length ok) > 0) arg_spec) in
	let cat_order = ["Target";"Compilation";"Optimization";"Debug";"Batch";"Services";"Compilation Server";"Target-specific";"Miscellaneous"] in
	let cats = List.filter (fun x -> List.mem x (List.map (fun (cat, _, _, _, _, _) -> cat) args)) cat_order in
	let max_length = List.fold_left max 0 (List.map String.length (List.map (fun (_, ok, _, _, hint, _) -> make_label ok hint) args)) in
	usage ^ (String.concat "\n" (List.flatten (List.map (fun cat -> (if print_cat then ["\n"^cat^":"] else []) @ (List.map (fun (cat, ok, dep, spec, hint, doc) ->
		let label = make_label ok hint in
		Printf.sprintf "  %s%s  %s" label (String.make (max_length - (String.length label)) ' ') doc
	) (List.filter (fun (cat', _, _, _, _, _) -> (if List.mem cat' cat_order then cat' else "Miscellaneous") = cat) args))) cats)))

let process_args arg_spec =
	List.flatten(List.map (fun (cat, ok, dep, spec, hint, doc) ->
		(* official argument names *)
		(List.map (fun (arg) -> (arg, spec, doc)) ok) @
		let dep_fun arg spec = () in
		let dep_spec arg spec = match spec with
			| Arg.String f -> Arg.String (fun x -> dep_fun arg spec; f x)
			| Arg.Unit f -> Arg.Unit (fun x -> dep_fun arg spec; f x)
			| Arg.Bool f -> Arg.Bool (fun x -> dep_fun arg spec; f x)
			| _ -> spec in
		(List.map (fun (arg) -> (arg, dep_spec arg spec, doc)) dep)
	) arg_spec)

let parse_args com =
	let usage = Printf.sprintf
		"Haxe Compiler %s - (C)2005-2022 Haxe Foundation\nUsage: haxe%s <target> [options] [hxml files and dot paths...]\n"
		s_version_full (if Sys.os_type = "Win32" then ".exe" else "")
	in
	let actx = {
		classes = [([],"Std")];
		xml_out = None;
		json_out = None;
		cmds = [];
		config_macros = [];
		no_output = false;
		did_something = false;
		force_typing = false;
		pre_compilation = [];
		interp = false;
		jvm_flag = false;
		swf_version = false;
		native_libs = [];
		raise_usage = (fun () -> ());
		display_arg = None;
		deprecations = [];
	} in
	let add_deprecation s =
		actx.deprecations <- s :: actx.deprecations
	in
	let add_native_lib file extern = actx.native_libs <- (file,extern) :: actx.native_libs in
	let 