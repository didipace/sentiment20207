open Globals
open Ast
open Common
open Filename
open Timer
open DisplayTypes.DisplayMode
open DisplayTypes.CompletionResultKind
open CompletionItem
open CompletionClassField
open CompletionEnumField
open ClassFieldOrigin
open DisplayException
open Type
open Display
open DisplayTypes
open CompletionModuleType
open Typecore
open Genjson
open CompilationContext
open DisplayProcessingGlobals

(* Old XML stuff *)

let htmlescape s =
	let s = String.concat "&amp;" (ExtString.String.nsplit s "&") in
	let s = String.concat "&lt;" (ExtString.String.nsplit s "<") in
	let s = String.concat "&gt;" (ExtString.String.nsplit s ">") in
	let s = String.concat "&quot;" (ExtString.String.nsplit s "\"") in
	s

let get_timer_fields start_time =
	let tot = ref 0. in
	Hashtbl.iter (fun _ t -> tot := !tot +. t.total) Timer.htimers;
	let fields = [("@TOTAL", Printf.sprintf "%.3fs" (get_time() -. start_time))] in
	if !tot > 0. then
		Hashtbl.fold (fun _ t acc ->
			((String.concat "." t.id),(Printf.sprintf "%.3fs (%.0f%%)" t.total (t.total *. 100. /. !tot))) :: acc
		) Timer.htimers fields
	else
		fields

let print_keywords () =
	let b = Buffer.create 0 in
	Buffer.add_string b "<list>\n";
	Hashtbl.iter (fun k _ ->
		Buffer.add_string b (Printf.sprintf "<i n=\"%s\"></i>\n" k)
	) Lexer.keywords;
	Buffer.add_string b "</list>\n";
	Buffer.contents b

let print_fields fields =
	let b = Buffer.create 0 in
	Buffer.add_string b "<list>\n";
	let convert k = match k.ci_kind with
		| ITClassField({field = cf}) | ITEnumAbstractField(_,{field = cf}) ->
			let kind = match cf.cf_kind with
				| Method _ -> "method"
				| Var _ -> "var"
			in
			kind,cf.cf_name,s_type (print_context()) cf.cf_type,cf.cf_doc
		| ITEnumField ef ->
			let ef = ef.efield in
			let kind = match follow ef.ef_type with
				| TFun _ -> "method"
				| _ -> "var"
			in
			kind,ef.ef_name,s_type (print_context()) ef.ef_type,ef.ef_doc
		| ITType(cm,_) ->
			let path = CompletionItem.CompletionModuleType.get_path cm in
			"type",snd path,s_type_path path,None
		| ITPackage(path,_) -> "package",snd path,"",None
		| ITModule path -> "type",snd path,"",None
		| ITMetadata  meta ->
			let s,(doc,_),_ = Meta.get_info meta in
			"metadata","@" ^ s,"",doc_from_string doc
		| ITTimer(name,value) -> "timer",name,"",doc_from_string value
		| ITLiteral s ->
			let t = match k.ci_type with None -> t_dynamic | Some (t,_) -> t in
			"literal",s,s_type (print_context()) t,None
		| ITLocal v -> "local",v.v_name,s_type (print_context()) v.v_type,None
		| ITKeyword kwd -> "keyword",Ast.s_keyword kwd,"",None
		| ITExpression _ | ITAnonymous _ | ITTypeParameter _ | ITDefine _ -> die "" __LOC__
	in
	let fields = List.sort (fun k1 k2 -> compare (legacy_sort k1) (legacy_sort k2)) fields in
	let fields = List.map convert fields in
	List.iter (fun(k,n,t,d) ->
		let d = match d with None -> "" | Some d -> gen_doc_text d in
		Buffer.add_string b (Printf.sprintf "<i n=\"%s\" k=\"%s\"><t>%s</t><d>%s</d></i>\n" n k (htmlescape t) (htmlescape d))
	) fields;
	Buffer.add_string b "</list>\n";
	Buffer.contents b

let maybe_print_doc d_opt =
	Option.map_default (fun d -> Printf.sprintf " d=\"%s\"" (htmlescape (gen_doc_text d))) "" d_opt

let print_toplevel il =
	let b = Buffer.create 0 in
	Buffer.add_string b "<il>\n";
	let s_type t = htmlescape (s_type (print_context()) t) in
	let s_doc d = maybe_print_doc d in
	let identifiers = Hashtbl.create 0 in
	let check_ident s =
		if Hashtbl.mem identifiers s then false
		else begin
			Hashtbl.add identifiers s true;
			true
		end
	in
	List.iter (fun id -> match id.ci_kind with
		| ITLocal v ->
			if check_ident v.v_name then Buffer.add_string b (Printf.sprintf "<i k=\"local\" t=\"%s\">%s</i>\n" (s_type v.v_type) v.v_name);
		| ITClassField({field = cf;scope = CFSMember}) ->
			if check_ident cf.cf_name then Buffer.add_string b (Printf.sprintf "<i k=\"member\" t=\"%s\"%s>%s</i>\n" (s_type cf.cf_type) (s_doc cf.cf_doc) cf.cf_name);
		| ITClassField({field = cf;scope = (CFSStatic | CFSConstructor)}) ->
			if check_ident cf.cf_name then Buffer.add_string b (Printf.sprintf "<i k=\"static\" t=\"%s\"%s>%s</i>\n" (s_type cf.cf_type) (s_doc cf.cf_doc) cf.cf_name);
		| ITEnumField ef ->
			let ef = ef.efield 