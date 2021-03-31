open Globals
open Json.Reader
open JsonRpc
open Jsonrpc_handler
open Json
open Common
open DisplayTypes.DisplayMode
open Timer
open Genjson
open Type
open DisplayProcessingGlobals

(* Generate the JSON of our times. *)
let json_of_times root =
	let rec loop node =
		if node == root || node.time > 0.0009 then begin
			let children = ExtList.List.filter_map loop node.children in
			let fl = [
				"name",jstring node.name;
				"path",jstring node.path;
				"info",jstring node.info;
				"time",jfloat node.time;
				"calls",jint node.num_calls;
				"percentTotal",jfloat (if root.time = 0. then 0. else (node.time *. 100. /. root.time));
				"percentParent",jfloat (if node == root || node.parent.time = 0. then 0. else node.time *. 100. /. node.parent.time);
			] in
			let fl = match children with
				| [] -> fl
				| _ -> ("children",jarray children) :: fl
			in
			Some (jobject fl)
		end else
			None
	in
	loop root

let supports_resolve = ref false

let create_json_context jsonrpc may_resolve =
	Genjson.create_context ~jsonrpc:jsonrpc (if may_resolve && !supports_resolve then GMMinimum else GMFull)

let send_string j =
	raise (Completion j)

let send_json json =
	send_string (string_of_json json)

class display_handler (jsonrpc : jsonrpc_handler) com (cs : CompilationCache.t) = object(self)
	val cs = cs;

	method get_cs = cs

	method enable_display mode =
		com.display <- create mode;
		Parser.display_mode := mode;
		Common.define_value com Define.Display "1"

	method set_display_file was_auto_triggered requires_offset =
		let file = jsonrpc#get_opt_param (fun () ->
			let file = jsonrpc#get_string_param "file" in
			Path.get_full_path file
		) file_input_marker in
		let pos = if requires_offset then jsonrpc#get_int_param "offset" else (-1) in
		TypeloadParse.current_stdin := jsonrpc#get_opt_param (fun () ->
			let s = jsonrpc#get_string_param "contents" in
			Common.define com Define.DisplayStdin; (* TODO: awkward *)
			Some s
		) None;
		Parser.was_auto_tr