open Globals
open Ast
open TType

let monomorph_create_ref : (unit -> tmono) ref = ref (fun _ -> die "" __LOC__)
let monomorph_bind_ref : (tmono -> t -> unit) ref = ref (fun _ _ -> die "" __LOC__)
let monomorph_classify_constraints_ref : (tmono -> tmono_constraint_kind) ref = ref (fun _ -> die "" __LOC__)

let has_meta m ml = List.exists (fun (m2,_,_) -> m = m2) ml
let get_meta m ml = List.find (fun (m2,_,_) -> m = m2) ml

(* Flags *)

let has_flag flags flag =
	flags land (1 lsl flag) > 0

let set_flag flags flag =
	flags lor (1 lsl flag)

let unset_flag flags flag =
	flags land (lnot (1 lsl flag))

let int_of_class_flag (flag : flag_tclass) =
	Obj.magic flag

let add_class_flag c (flag : flag_tclass) =
	c.cl_flags <- set_flag c.cl_flags (int_of_class_flag flag)

let remove_class_flag c (flag : flag_tclass) =
	c.cl_flags <- unset_flag c.cl_flags (int_of_class_flag flag)

let has_class_flag c (flag : flag_tclass) =
	has_flag c.cl_flags (int_of_class_flag flag)

let int_of_class_field_flag (flag : flag_tclass_field) =
	Obj.magic flag

let add_class_field_flag cf (flag : flag_tclass_field) =
	cf.cf_flags <- set_flag cf.cf_flags (int_of_class_field_flag flag)

let remove_class_field_flag cf (flag : flag_tclass_field) =
	cf.cf_flags <- unset_flag cf.cf_flags (int_of_class_field_flag flag)

let has_class_field_flag cf (flag : flag_tclass_field) =
	has_flag cf.cf_flags (int_of_class_field_flag flag)

let int_of_var_flag (flag : flag_tvar) =
	Obj.magic flag

let add_var_flag v (flag : flag_tvar) =
	v.v_flags <- set_flag v.v_flags (int_of_var_flag flag)

let remove_var_flag v (flag : flag_tvar) =
	v.v_flags <- unset_flag v.v_flags (int_of_var_flag flag)

let has_var_flag v (flag : flag_tvar) =
	has_flag v.v_flags (int_of_var_flag flag)

(* ======= General utility ======= *)

let alloc_var =
	let uid = ref 0 in
	(fun kind n t p ->
		incr uid;
		{
			v_kind = kind;
			v_name = n;
			v_type = t;
			v_id = !uid;
			v_extra = None;
			v_meta = [];
			v_pos = p;
			v_flags = (match kind with VUser TVOLocalFunction -> int_of_var_flag VFinal | _ -> 0);
		}
	)

let alloc_mid =
	let mid = ref 0 in
	(fun() -> incr mid; !mid)

let mk e t p = { eexpr = e; etype = t; epos = p }

let mk_block e =
	match e.eexpr with
	| TBlock _ -> e
	| _ -> mk (TBlock [e]) e.etype e.epos

let mk_cast e t p = mk (TCast(e,None)) t p

let null t p = mk (TConst TNull) t p

let mk_mono() = TMono (!monomorph_create_ref ())

let rec t_dynamic = TDynamic t_dynamic

let mk_anon ?fields status =
	let fields = match fields with Some fields -> fields | None -> PMap.empty in
	TAnon { a_fields = fields; a_status = status; }

(* We use this for display purposes because otherwise we never see the Dynamic type that
   is defined in StdTypes.hx. This is set each time a typer is created, but this is fine
   because Dynamic is the same in all contexts. If this ever changes we'll have to review
   how we handle this. *)
let t_dynamic_def = ref t_dynamic

let tfun pl r = TFun (List.map (fun t -> "",false,t) pl,r)

let fun_args l = List.map (fun (a,c,t) -> a, c <> None, t) l

let mk_class m path pos name_pos =
	{
		cl_path = path;
		cl_module = m;
		cl_pos = pos;
		cl_name_pos = name_pos;
		cl_doc = None;
		cl_meta = [];
		cl_private = false;
		cl_kind = KNormal;
		cl_flags = 0;
		cl_params = [];
		cl_using = [];
		cl_super = None;
		cl_implements = [];
		cl_fields = PMap.empty;
		cl_ordered_statics = [];
		cl_ordered_fields = [];
		cl_statics = PMap.empty;
		cl_dynamic = None;
		cl_array_access = None;
		cl_constructor = None;
		cl_init = None;
		cl_build = (fun() -> Built);
		cl_restore = (fun() -> ());
		cl_descendants = [];
	}

let mk_typedef m path pos name_pos t =
	{
		t_path = path;
		t_module = m;
		t_pos = pos;
		t_name_pos = name_pos;
		t_private = false;
		t_doc = None;
		t_meta = [];
		t_params = [];
		t_using = [];
		t_type = t;
		t_restore = (fun () -> ());
	}

let module_extra file sign time kind policy =
	{
		m_file = Path.UniqueKey.create_lazy file;
		m_sign = sign;
		m_display = {
			m_inline_calls = [];
			m_type_hints = [];
			m_import_positions = PMap.empty;
		};
		m_cache_state = MSGood;
		m_added = 0;
		m_checked = 0;
		m_time = time;
		m_processed = 0;
		m_deps = PMap.empty;
		m_kind = kind;
		m_binded_res = PMap.empty;
		m_if_feature = [];
		m_features = Hashtbl.create 0;
		m_check_policy = policy;
	}


let mk_field name ?(public = true) ?(static = false) t p name_pos = {
	cf_name = name;
	cf_type = t;
	cf_pos = p;
	cf_name_pos = name_pos;
	cf_doc = None;
	cf_meta = [];
	cf_kind = Var { v_read = AccNormal; v_write = AccNormal };
	cf_expr = None;
	cf_expr_unoptimized = None;
	cf_params = [];
	cf_overloads = [];
	cf_flags = (
		let flags = if static then set_flag 0 (int_of_class_field_flag CfStatic) else 0 in
		if public then set_flag flags (int_of_class_field_flag CfPublic) else flags
	);
}

let null_module = {
		m_id = alloc_mid();
		m_path = [] , "";
		m_types = [];
		m_statics = None;
		m_extra = module_extra "" "" 0. MFake [];
	}

let null_class =
	let c = mk_class null_module ([],"") null_pos null_pos in
	c.cl_private <- true;
	c

let null_field = mk_field "" t_dynamic null_pos null_pos

let null_abstract = {
	a_path = ([],"");
	a_module = null_module;
	a_pos = null_pos;
	a_name_pos = null_pos;
	a_private = true;
	a_doc = None;
	a_meta = [];
	a_params = [];
	a_using = [];
	a_restore = (fun () -> ());
	a_ops = [];
	a_unops = [];
	a_impl = None;
	a_this = t_dynamic;
	a_from = [];
	a_from_field = [];
	a_to = [];
	a_to_field = [];
	a_array = [];
	a_read = None;
	a_write = None;
	a_call = None;
	a_enum = false;
}

let add_dependency ?(skip_postprocess=false) m mdep =
	if m != null_module && m != mdep then begin
		m.m_extra.m_deps <- PMap.add mdep.m_id mdep m.m_extra.m_deps;
		(* In case the module is cached, we'll have to run post-processing on it again (issue #10635) *)
		if not skip_postprocess then m.m_extra.m_processed <- 0
	end

let arg_name (a,_) = a.v_name

let t_infos t : tinfos =
	match t with
	| TClassDecl c -> Obj.magic c
	| TEnumDecl e -> Obj.magic e
	| TTypeDecl t -> Obj.magic t
	| TAbstractDecl a -> Obj.magic a

let t_path t = (t_infos t).mt_path

let rec extends c csup =
	if c == csup || List.exists (fun (i,_) -> extends i csup) c.cl_implements then
		true
	else match c.cl_super with
		| None -> false
		| Some (c,_) -> extends c csup

let add_descendant c descendant =
	c.cl_descendants <- descendant :: c.cl_descendants

let lazy_type f =
	match !f with
	| LAvailable t -> t
	| LProcessing f | LWait f -> f()

let lazy_available t = LAvailable t
let lazy_processing f = LProcessing f
let lazy_wait f = LWait f

let map loop t =
	match t with
	| TMono r ->
		(match r.tm_type with
		| None -> t
		| Some t -> loop t) (* erase*)
	| TEnum (_,[]) | TInst (_,[]) | TType (_,[]) | TAbstract (_,[]) ->
		t
	| TEnum (e,tl) ->
		TEnum (e, List.map loop tl)
	| TInst (c,tl) ->
		TInst (c, List.map loop tl)
	| TType (t2,tl) ->
		TType (t2,List.map loop tl)
	| TAbstract (a,tl) ->
		TAbstract (a,List.map loop tl)
	| TFun (tl,r) ->
		TFun (List.map (fun (s,o,t) -> s, o, loop t) tl,loop r)
	| TAnon a ->
		let fields = PMap.map (fun f -> { f with cf_type = loop f.cf_type }) a.a_fields in
		mk_anon ~fields a.a_status
	| TLazy f ->
		let ft = lazy_type f in
		let ft2 = loop ft in
		if ft == ft2 then t else ft2
	| TDynamic t2 ->
		if t == t2 then	t else TDynamic (loop t2)

let iter loop t =
	match t with
	| TMono r ->
		(match r.tm_type with
		| None -> ()
		| Some t -> loop t)
	| TEnum (_,[]) | TInst (_,[]) | TType (_,[]) ->
		()
	| TEnum (e,tl) ->
		List.iter loop tl
	| TInst (c,tl) ->
		List.iter loop tl
	| TType (t2,tl) ->
		List.iter loop tl
	| TAbstract (a,tl) ->
		List.iter loop tl
	| TFun (tl,r) ->
		List.iter (fun (_,_,t) -> loop t) tl;
		loop r
	| TAnon a ->
		PMap.iter (fun _ f -> loop f.cf_type) a.a_fields
	| TLazy f ->
		let ft = lazy_type f in
		loop ft
	| TDynamic t2 ->
		if t != t2 then	loop t2

let duplicate t =
	let monos = ref [] in
	let rec loop t =
		match t with
		| TMono { tm_type = None } ->
			(try
				List.assq t !monos
			with Not_found ->
				let m = mk_mono() in
				monos := (t,m) :: !monos;
				m)
		| _ ->
			map loop t
	in
	loop t

let dynamify_monos t =
	let rec loop t =
		match t with
		| TMono { tm_type = None } ->
			t_dynamic