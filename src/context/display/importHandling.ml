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
			| (_,p) :: _ -> Error.typing_error "Module name must start with an uppercase letter" p))
	| (tname,p2) :: rest ->
		let p1 = (match pack with [] -> p2 | (_,p1) :: _ -> p1) in
		let p_type = punion p1 p2 in
		let md = ctx.g.do_load_module ctx (List.map fst pack,tname) p_type in
		let types = md.m_types in
		let no_private (t,_) = not (t_infos t).mt_private in
		let error_private p = typing_error "Importing private declarations from a module is not allowed" p in
		let chk_private t p = if ctx.m.curmod != (t_infos t).mt_module && (t_infos t).mt_private then error_private p in
		let has_name name t = snd (t_infos t).mt_path = name in

		let fail_usefully name p =
			let target_kind,candidates = match String.get name 0 with
				(* TODO: cleaner way to get module fields? *)
				| 'a'..'z' -> "field", PMap.foldi (fun n _ acc -> n :: acc) (try (Option.get md.m_statics).cl_statics with | _ -> PMap.empty) []
				| _ -> "type", List.map (fun mt -> snd (t_infos mt).mt_path) types
			in
			typing_error (StringError.string_error name
				candidates
				("Module " ^ s_type_path md.m_path ^ " does not define " ^ target_kind ^ " " ^ name)
			) p
		in

		let find_type tname = List.find (has_name tname) types in
		let get_type tname =
			let t = try
				find_type tname
			with Not_found ->
				fail_usefully tname p_type
			in
			chk_private t p_type;
			t
		in
		let rebind t name p =
			if not (name.[0] >= 'A' && name.[0] <= 'Z') then
				typing_error "Type aliases must start with an uppercase letter" p;
			let _, _, f = ctx.g.do_build_instance ctx t p_type in
			(* create a temp private typedef, does not register it in module *)
			let t_path = (fst md.m_path @ ["_" ^ snd md.m_path],name) in
			let t_type = f (extract_param_types (t_infos t).mt_params) in
			let mt = TTypeDecl {(mk_typedef ctx.m.curmod t_path p p t_type) with
				t_private = true;
				t_params = (t_infos t).mt_params
			} in
			if ctx.is_display_file && DisplayPosition.display_position#enclosed_in p then
				DisplayEmitter.display_module_type ctx mt p;
			mt
		in
		let add_static_init t name s =
			let name = (match name with None -> s | Some (n,_) -> n) in
			match resolve_typedef t with
			| TClassDecl c | TAbstractDecl {a_impl = Some c} ->
				ignore(c.cl_build());
				ignore(PMap.find s c.cl_statics);
				ctx.m.module_globals <- PMap.add name (TClassDecl c,s,p) ctx.m.module_globals
			| TEnumDecl e ->
				ignore(PMap.find s e.e_constrs);
				ctx.m.module_globals <- PMap.add name (TEnumDecl e,s,p) ctx.m.module_globals
			| _ ->
				raise Not_found
		in
		(match mode with
		| INormal | IAsName _ ->
			let name = (match mode with IAsName n -> Some n | _ -> None) in
			(match rest with
			| [] ->
				(match name with
				| None ->
					ctx.m.module_imports <- List.filter no_private (List.map (fun t -> t,p) types) @ ctx.m.module_imports;
					Option.may (fun c ->
						context_init#add (fun () ->
							ignore(c.cl_build());
							List.iter (fun cf ->
								if has_class_field_flag cf CfPublic then
									ctx.m.module_globals <- PMap.add cf.cf_name (TClassDecl c,cf.cf_name,p) ctx.m.module_globals
							) c.cl_ordered_statics
						);
					) md.m_statics
				| Some(newname,pname) ->
					ctx.m.module_imports <- (rebi