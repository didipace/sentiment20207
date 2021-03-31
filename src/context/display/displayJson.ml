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
		Parser.was_auto_triggered := was_auto_triggered;
		DisplayPosition.display_position#set {
			pfile = file;
			pmin = pos;
			pmax = pos;
		}
end

type handler_context = {
	com : Common.context;
	jsonrpc : jsonrpc_handler;
	display : display_handler;
	send_result : Json.t -> unit;
	send_error : 'a . Json.t list -> 'a;
}

let handler =
	let open CompilationCache in
	let h = Hashtbl.create 0 in
	let l = [
		"initialize", (fun hctx ->
			supports_resolve := hctx.jsonrpc#get_opt_param (fun () -> hctx.jsonrpc#get_bool_param "supportsResolve") false;
			DisplayException.max_completion_items := hctx.jsonrpc#get_opt_param (fun () -> hctx.jsonrpc#get_int_param "maxCompletionItems") 0;
			let exclude = hctx.jsonrpc#get_opt_param (fun () -> hctx.jsonrpc#get_array_param "exclude") [] in
			DisplayToplevel.exclude := List.map (fun e -> match e with JString s -> s | _ -> die "" __LOC__) exclude;
			let methods = Hashtbl.fold (fun k _ acc -> (jstring k) :: acc) h [] in
			hctx.send_result (JObject [
				"methods",jarray methods;
				"haxeVersion",jobject [
					"major",jint version_major;
					"minor",jint version_minor;
					"patch",jint version_revision;
					"pre",(match version_pre with None -> jnull | Some pre -> jstring pre);
					"build",(match Version.version_extra with None -> jnull | Some(_,build) -> jstring build);
				];
				"protocolVersion",jobject [
					"major",jint 0;
					"minor",jint 5;
					"patch",jint 0;
				]
			])
		);
		"display/completionItem/resolve", (fun hctx ->
			let i = hctx.jsonrpc#get_int_param "index" in
			begin try
				let item = (!DisplayException.last_completion_result).(i) in
				let ctx = Genjson.create_context GMFull in
				hctx.send_result (jobject ["item",CompletionItem.to_json ctx None item])
			with Invalid_argument _ ->
				hctx.send_error [jstring (Printf.sprintf "Invalid index: %i" i)]
			end
		);
		"display/completion", (fun hctx ->
			hctx.display#set_display_file (hctx.jsonrpc#get_bool_param "wasAutoTriggered") true;
			hctx.display#enable_display DMDefault;
		);
		"display/definition", (fun hctx ->
			hctx.display#set_display_file false true;
			hctx.display#enable_display DMDefinition;
		);
		"display/implementation", (fun hctx ->
			hctx.display#set_display_file false true;
			hctx.display#enable_display (DMImplementation);
		);
		"display/typeDefinition", (fun hctx ->
			hctx.display#set_display_file false true;
			hctx.display#enable_display DMTypeDefinition;
		);
		"display/references", (fun hctx ->
			hctx.display#set_display_file false true;
			match hctx.jsonrpc#get_opt_param (fun () -> hctx.jsonrpc#get_string_param "kind") "normal" with
			| "withBaseAndDescendants" ->
				hctx.display#enable_display (DMUsage (false,true,true));
			| "withDescendants" ->
				hctx.display#enable_display (DMUsage (false,true,false));
			| _ ->
				hctx.display#enable_display (DMUsage (false,false,false));
		);
		"display/hover", (fun hctx ->
			hctx.display#set_display_file false true;
			hctx.display#enable_display DMHover;
		);
		"display/package", (fun hctx ->
			hctx.display#set_display_file false fals