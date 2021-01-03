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
	let basic_args_spec = [
		("Target",["--js"],["-js"],Arg.String (set_platform com Js),"<file>","generate JavaScript code into target file");
		("Target",["--lua"],["-lua"],Arg.String (set_platform com Lua),"<file>","generate Lua code into target file");
		("Target",["--swf"],["-swf"],Arg.String (set_platform com Flash),"<file>","generate Flash SWF bytecode into target file");
		("Target",["--neko"],["-neko"],Arg.String (set_platform com Neko),"<file>","generate Neko bytecode into target file");
		("Target",["--php"],["-php"],Arg.String (fun dir ->
			actx.classes <- (["php"],"Boot") :: actx.classes;
			set_platform com Php dir;
		),"<directory>","generate PHP code into target directory");
		("Target",["--cpp"],["-cpp"],Arg.String (fun dir ->
			set_platform com Cpp dir;
		),"<directory>","generate C++ code into target directory");
		("Target",["--cppia"],["-cppia"],Arg.String (fun file ->
			Common.define com Define.Cppia;
			set_platform com Cpp file;
		),"<file>","generate Cppia bytecode into target file");
		("Target",["--cs"],["-cs"],Arg.String (fun dir ->
			set_platform com Cs dir;
		),"<directory>","generate C# code into target directory");
		("Target",["--java"],["-java"],Arg.String (fun dir ->
			set_platform com Java dir;
		),"<directory>","generate Java code into target directory");
		("Target",["--jvm"],[],Arg.String (fun dir ->
			Common.define com Define.Jvm;
			actx.jvm_flag <- true;
			set_platform com Java dir;
		),"<file>","generate JVM bytecode into target file");
		("Target",["--python"],["-python"],Arg.String (fun dir ->
			set_platform com Python dir;
		),"<file>","generate Python code into target file");
		("Target",["--hl"],["-hl"],Arg.String (fun file ->
			set_platform com Hl file;
		),"<file>","generate HashLink .hl bytecode or .c code into target file");
		("Target",[],["-x"], Arg.String (fun cl ->
			let cpath = Path.parse_type_path cl in
			(match com.main_class with
				| Some c -> if cpath <> c then raise (Arg.Bad "Multiple --main classes specified")
				| None -> com.main_class <- Some cpath);
			actx.classes <- cpath :: actx.classes;
			Common.define com Define.Interp;
			set_platform com (!Globals.macro_platform) "";
			actx.interp <- true;
		),"<class>","interpret the program using internal macro system");
		("Target",["--interp"],[], Arg.Unit (fun() ->
			Common.define com Define.Interp;
			set_platform com (!Globals.macro_platform) "";
			actx.interp <- true;
		),"","interpret the program using internal macro system");
		("Target",["--run"],[], Arg.Unit (fun() ->
			raise (Arg.Bad "--run requires an argument: a Haxe module name")
		), "<module> [args...]","interpret a Haxe module with command line arguments");
		("Compilation",["-p";"--class-path"],["-cp"],Arg.String (fun path ->
			com.class_path <- Path.add_trailing_slash path :: com.class_path
		),"<path>","add a directory to find source files");
		("Compilation",["-m";"--main"],["-main"],Arg.String (fun cl ->
			if com.main_class <> None then raise (Arg.Bad "Multiple --main classes specified");
			let cpath = Path.parse_type_path cl in
			com.main_class <- Some cpath;
			actx.classes <- cpath :: actx.classes
		),"<class>","select startup class");
		("Compilation",["-D";"--define"],[],Arg.String (fun var ->
			let flag, value = try let split = ExtString.String.split var "=" in (fst split, Some (snd split)) with _ -> var, None in
			match value with
				| Some value -> Common.external_define_value com flag value
				| None -> Common.external_define com flag;
		),"<var[=value]>","define a conditional compilation flag");
		("Debug",["-v";"--verbose"],[],Arg.Unit (fun () ->
			com.verbose <- true
		),"","turn on verbose mode");
		("Debug",["--debug"],["-debug"], Arg.Unit (fun() ->
			Common.define com Define.Debug;
			com.debug <- true;
		),"","add debug information to the compiled code");
		("Miscellaneous",["--version"],["-version"],Arg.Unit (fun() ->
			com.info s_version_full null_pos;
			actx.did_something <- true;
		),"","print version and exit");
		("Miscellaneous", ["-h";"--help"], ["-help"], Arg.Unit (fun () ->
			raise (Arg.Help "")
		),"","show extended help information");
		("Miscellaneous",["--help-defines"],[], Arg.Unit (fun() ->
			let all,max_length = Define.get_documentation_list com.user_defines in
			let all = List.map (fun (n,doc) -> Printf.sprintf " %-*s: %s" max_length n (limit_string doc (max_length + 3))) all in
			List.iter (fun msg -> com.print (msg ^ "\n")) all;
			actx.did_something <- true
		),"","print help for all compiler specific defines");
		("Miscellaneous",["--help-user-defines"],[], Arg.Unit (fun() ->
			actx.did_something <- true;
			com.callbacks#add_after_init_macros (fun() ->
				let all,max_length = Define.get_user_documentation_list com.user_defines in
				let all = List.map (fun (n,doc) -> Printf.sprintf " %-*s: %s" max_length n (limit_string doc (max_length + 3))) all in
				List.iter (fun msg -> com.print (msg ^ "\n")) all;
				exit 0
			)
		),"","print help for all user defines");
		("Miscellaneous",["--help-metas"],[], Arg.Unit (fun() ->
			let all,max_length = Meta.get_documentation_list com.user_metas in
			let all = List.map (fun (n,doc) -> Printf.sprintf " %-*s: %s" max_length n (limit_string doc (max_length + 3))) all in
			List.iter (fun msg -> com.print (msg ^ "\n")) all;
			actx.did_something <- true
		),"","print help for all compiler metadatas");
		("Miscellaneous",["--help-user-metas"],[], Arg.Unit (fun() ->
			actx.did_something <- true;
			com.callbacks#add_after_init_macros (fun() ->
				let all,max_length = Meta.get_user_documentation_list com.user_metas in
				let all = List.map (fun (n,doc) -> Printf.sprintf " %-*s: %s" max_length n (limit_string doc (max_length + 3))) all in
				List.iter (fun msg -> com.print (msg ^ "\n")) all;
				exit 0
			)
		),"","print help for all user metadatas");
	] in
	let adv_args_spec = [
		("Optimization",["--dce"],["-dce"],Arg.String (fun mode ->
			(match mode with
			| "std" | "full" | "no" -> ()
			| _ -> raise (Arg.Bad "Invalid DCE mode, expected