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
	