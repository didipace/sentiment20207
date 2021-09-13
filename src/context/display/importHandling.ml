open Globals
open Ast
open DisplayPosition
open Common
open Type
open Error
open Typecore

type import_display_kind =
	| IDKPackage of string list
	| IDKModule of string list * string
	| IDKSubType of string list * string * string
	| IDKModuleField of string list * string * string
	| IDKSubTypeField of string list * string * string * string
	| IDK

type import_display = import_display_kind * pos

let convert_import_to_something_usable pt path =
	let rec loop pack m t = function
		| (s,p) :: l ->
			let is_lower = is_lower_ident s in
			let is_display_pos = encloses_position pt p in
			begin match is_lower,m,t with
				| _,None,Some _ ->
					die "" __LOC__ (* impossible, I think *)
				| true,Some m,None ->
					if is_display_pos then (IDKModuleField(List.rev pack,m,s),p)
					else (IDK,p) (* assume that we're done *)
				| _,Some m,Some t ->
					if is_display_pos then (IDKSubTypeField(List.rev pack,m,t,s),p)
					else (IDK,p)
				| true,None,None ->
					if is_display_pos then (IDKPackage (List.rev (s :: pack)),p)
					else loop (s :: pack) m t l
				| false,Some sm,None ->
					if is_display_pos then (IDKSubType (List.rev pack,sm,s),p)
					else loop pack m (Some s) l
				| false,None,None ->
					if is_display_pos then (IDKModule (List.rev pack,s),p)
					else loop pack (Some s) None l
			end
		| [] ->
			(IDK,null_pos)
	in
	loop [] None None path

let add_import_position ctx p path =
	let infos = ctx.m.curmod.m_extra.m_display in
	if not (PMap.mem p infos.m_import_positions) then
		infos.m_import_positions <- PMap.add p (ref false) infos.m_import_positions

let mark_import_position ctx p =
	try
		let r = PMap.find p ctx.m.curmod.m_extra.m_display.m_import_positions in
		r := true
	with Not_found ->
		()

let commit_import ctx path mode p =
	ctx.m.import_statements <- (path,mode) :: ctx.m.import_statements;
	if Filename.basename p.pfile <> "import.hx" then add_import_position ctx p path

let init_import ctx context_init path mode p =
	let rec loop acc = function
		| x :: l when is_lower_ident (fst x) -> loop (x::acc) l
		| rest -> List.rev acc, rest
	in
	let pack, rest = loop [] path in
	(match rest with
	| [] ->
		(match mode with
		| IAll ->
			ctx.m.wildcard_packages <- (List.map fst pack,p) :: ctx.m.wildcard_packages
		| _ ->
			(match List.rev path with
			(* p spans `import |` (to the display position), so we take the pmax here *)
			| [] -> DisplayException.raise_fields (DisplayToplevel.collect ctx TKType NoValue true) CRImport (DisplayTypes.make_subject None {p with pmin = p.pmax})
			| (_,p) :: _ -> Error.typi