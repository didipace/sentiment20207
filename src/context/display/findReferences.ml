open Globals
open Ast
open DisplayTypes
open Common
open Type
open Typecore
open ImportHandling

let find_possible_references tctx cs =
	let name,_,kind = Display.ReferencePosition.get () in
	ignore(SyntaxExplorer.explore_uncached_modules tctx cs [name,kind])

let find_references tctx com with_definition pos_filters =
	let t = Timer.timer ["display";"references";"collect"] in
	let symbols,relations = Statistics.collect_statistics tctx pos_filters true in
	t();
	let rec loop acc (relations:(Statistics.relation * pos) list) = match relations with
		| (Statistics.Referenced,p) :: relations when not (List.mem p acc) -> loop (p :: acc) relations
		| _ :: relations -> loop acc relations
		| [] -> acc
	in
	let t = Timer.timer ["display";"references";"filter"] in
	let usages = Hashtbl.fold (fun p sym acc ->
		let acc = if with_definition then p :: acc else acc in
		(try loop acc (Hashtbl.find relations p)
		with Not_found -> acc)
	) symbols [] in
	t();
	Display.ReferencePosition.reset();
	usages

let collect_reference_positions com =
	let name,pos,kind = Display.ReferencePosition.get () in
	match kind, com.display.dms_kind with
	| SKField (cf,Some cl_path), DMUsage (_,find_descendants,find_base) when (find_descendants || find_base) && not (has_cl