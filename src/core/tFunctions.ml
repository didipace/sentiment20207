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
	a_path = ([],"