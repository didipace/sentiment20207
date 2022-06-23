
(*
 * Copyright (C)2005-2013 Haxe Foundation
 *
 * Permission is hereby granted, free of charge, to any person obtaining a
 * copy of this software and associated documentation files (the "Software"),
 * to deal in the Software without restriction, including without limitation
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,
 * and/or sell copies of the Software, and to permit persons to whom the
 * Software is furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
 * DEALINGS IN THE SOFTWARE.
 *)

open Ast
open Type
open Common
open ExtList
open Error

type pos = Globals.pos

type ctx = {
    com : Common.context;
    buf : Buffer.t;
    packages : (string list,unit) Hashtbl.t;
    mutable current : tclass;
    mutable statics : (tclass * tclass_field * texpr) list;
    mutable inits : texpr list;
    mutable tabs : string;
    mutable in_value : tvar option;
    mutable in_loop : bool;
    mutable in_loop_try : bool;
    mutable iife_assign : bool;
    mutable break_depth : int;
    mutable handle_continue : bool;
    mutable id_counter : int;
    mutable type_accessor : module_type -> string;
    mutable separator : bool;
    mutable found_expose : bool;
    mutable lua_jit : bool;
    mutable lua_vanilla : bool;
    mutable lua_ver : float;
}

type object_store = {
    os_name : string;
    mutable os_fields : object_store list;
}

let replace_float_separators s =  Texpr.replace_separators s ""

let debug_expression expression  =
    " --[[ " ^ Type.s_expr_kind expression  ^ " --]] "

let debug_type t  =
    " --[[ " ^ Type.s_type_kind t  ^ " --]] ";;

let flat_path (p,s) =
    (* Replace _ with __ in paths to prevent name collisions. *)
    let escape str = String.concat "_" (ExtString.String.nsplit str "_") in

    match p with
    | [] -> escape s
    | _ -> "__" ^ String.concat "_" (List.map escape p) ^ "_" ^ (escape s)

let get_exposed ctx path meta = try
        let (_, args, pos) = Meta.get Meta.Expose meta in
        (match args with
         | [ EConst (String(s,_)), _ ] -> [s]
         | [] -> [path]
         | _ -> typing_error "Invalid @:expose parameters" pos)
    with Not_found -> []

let dot_path = Globals.s_type_path

let s_path ctx = flat_path

(* Lua requires decimal encoding for characters, rather than the hex *)
(* provided by StringHelper.s_escape *)
let s_escape_lua ?(dec=true) s =
    let b = Buffer.create (String.length s) in
    for i = 0 to (String.length s) - 1 do
        match s.[i] with
        | '\n' -> Buffer.add_string b "\\n"
        | '\t' -> Buffer.add_string b "\\t"
        | '\r' -> Buffer.add_string b "\\r"
        | '"' -> Buffer.add_string b "\\\""
        | '\\' -> Buffer.add_string b "\\\\"
        | c when int_of_char c < 32 && dec ->
            Buffer.add_string b (Printf.sprintf "\\%.3d" (int_of_char c))
        | c -> Buffer.add_char b c
    done;
    Buffer.contents b

(* TODO: are all these kwds necessary for field quotes *and* id escapes? *)
let kwds =
    let h = Hashtbl.create 0 in
    List.iter (fun s -> Hashtbl.add h s ()) [
        "_G"; ""; "and"; "break"; "do"; "else"; "elseif";
        "end"; "false"; "for"; "function"; "if";
        "in"; "local"; "nil"; "not"; "or"; "repeat";
        "return"; "then"; "true"; "until"; "while";
        "goto"; "self";
    ];
    h

let valid_lua_ident s =
    try
        for i = 0 to String.length s - 1 do
            match String.unsafe_get s i with
            | 'a'..'z' | 'A'..'Z' | '_' -> ()
            | '0'..'9' when i > 0 -> ()
            | _ -> raise Exit
        done;
        true
    with Exit ->
        false

let field s = if Hashtbl.mem kwds s || not (valid_lua_ident s) then "[\"" ^ s ^ "\"]" else "." ^ s
let ident s = if Hashtbl.mem kwds s then "_" ^ s else s

let anon_field s = if Hashtbl.mem kwds s || not (valid_lua_ident s) then "['" ^ (s_escape_lua s) ^ "']" else s
let static_field c s =
    match s with
    | "length" | "name" when not (has_class_flag c CExtern) || Meta.has Meta.HxGen c.cl_meta-> "._hx" ^ s
    | s -> field s

let has_feature ctx = Common.has_feature ctx.com
let add_feature ctx = Common.add_feature ctx.com

let temp ctx =
    ctx.id_counter <- ctx.id_counter + 1;
    "_hx_" ^ string_of_int (ctx.id_counter)

let spr ctx s =
    ctx.separator <- false;
    Buffer.add_string ctx.buf s

let print ctx =
    ctx.separator <- false;
    Printf.kprintf (fun s -> Buffer.add_string ctx.buf s)

let newline ctx = print ctx "\n%s" ctx.tabs

(* print with newline *)
let println ctx =
    ctx.separator <- false;
    Printf.kprintf (fun s -> begin
            Buffer.add_string ctx.buf s;
            newline ctx
        end)

let unsupported p = typing_error "This expression cannot be compiled to Lua" p

let basename path =
    try
        let idx = String.rindex path '/' in
        String.sub path (idx + 1) (String.length path - idx - 1)
    with Not_found -> path

(* TODO : is this necessary any more?*)
let newprop ctx =
    match Buffer.nth ctx.buf (Buffer.length ctx.buf - 1) with
    | '{' -> print ctx "\n%s" ctx.tabs
    | _ -> print ctx "\n%s" ctx.tabs

let semicolon ctx =
    match Buffer.nth ctx.buf (Buffer.length ctx.buf - 1) with
    | '}' when not ctx.separator -> ()
    | _ -> spr ctx ";"

let rec concat ctx s f = function
    | [] -> ()
    | [x] -> f x
    | x :: l ->
        f x;
        spr ctx s;
        concat ctx s f l

(* create a __lua__ call *)
let mk_lua_code com code args t pos =
    let lua_local = mk (TIdent "__lua__") t_dynamic pos in
    let code_const = Texpr.Builder.make_string com code pos in
    mk (TCall (lua_local, code_const :: args)) t pos

let inject_rest_args ctx args e =
    match List.rev args with
    | (v,_) :: _ when ExtType.is_rest (follow v.v_type) ->
        let rest = mk (TLocal v) v.v_type v.v_pos in
        let init_rest = mk_lua_code ctx.com.basic "local {0} = {...}" [rest] ctx.com.basic.tvoid v.v_pos in
        (match e.eexpr with
        | TBlock el ->
            { e with eexpr = TBlock (init_rest :: el) }
        | _ ->
            { e with eexpr = TBlock [init_rest; e] }
        )
    | _ -> e

let fun_block ctx f p =
    let fn_body = inject_rest_args ctx f.tf_args f.tf_expr in
    List.fold_left (fun e (a,c) ->
        match c with
        | None | Some {eexpr = TConst TNull} -> e
        | Some c -> Type.concat (Texpr.set_default ctx.com.basic a c p) e
    ) fn_body f.tf_args

let open_block ctx =
    let oldt = ctx.tabs in
    ctx.tabs <- "  " ^ ctx.tabs;
    (fun() -> ctx.tabs <- oldt)

let this ctx = match ctx.in_value with None -> "self" | Some _ -> "self"

let is_dot_access e cf =
    match follow(e.etype), cf with
    | TInst (c, _), FInstance(_, _, icf) -> (match icf.cf_kind with
        | Var _ ->
            true
        | Method _ when Meta.has Meta.LuaDotMethod c.cl_meta ->
            true
        | Method _ when Meta.has Meta.LuaDotMethod icf.cf_meta ->
            true
        | Method _ ->
            false)
    | _ ->
        false

(*
	return index of a first element in the list for which `f` returns true
	TODO: is there some standard function to do that?
 *)
let index_of f l =
    let rec find lst idx =
        match lst with
        | [] -> raise Not_found
        | el :: rest ->
            if f el then
                idx
            else
                find rest (idx + 1)
    in
    find l 0

(* create a multi-return boxing call for given expr *)
let mk_mr_box ctx e =
    let s_fields =
        match follow e.etype with
        | TInst (c,_) ->
            String.concat ", " (List.map (fun f -> "\"" ^ f.cf_name ^ "\"") c.cl_ordered_fields)
        | _ -> Globals.die "" __LOC__
    in
    add_feature ctx "use._hx_box_mr";
    add_feature ctx "use._hx_table";
    let code = Printf.sprintf "_hx_box_mr(_hx_table.pack({0}), {%s})" s_fields in
    mk_lua_code ctx.com.basic code [e] e.etype e.epos

(* create a multi-return select call for given expr and field name *)
let mk_mr_select com e ecall name =
    let i =
        match follow ecall.etype with
        | TInst (c,_) ->
            index_of (fun f -> f.cf_name = name) c.cl_ordered_fields
        | _ ->
            Globals.die "" __LOC__
    in
    if i == 0 then
        mk_lua_code com "{0}" [ecall] e.etype e.epos
    else
        let code = Printf.sprintf "_G.select(%i, {0})" (i + 1) in
        mk_lua_code com code [ecall] e.etype e.epos

(* from genphp *)
let rec is_string_type t =
    match follow t with
    | TInst ({cl_kind = KTypeParameter constraints}, _) -> List.exists is_string_type constraints
    | TInst ({cl_path = ([], "String")}, _) -> true
    | TAnon a ->
        (match !(a.a_status) with
         | Statics ({cl_path = ([], "String")}) -> true
         | _ -> false)
    | TAbstract (a,pl) -> is_string_type (Abstract.get_underlying_type a pl)
    | _ -> false

let is_string_expr e = is_string_type e.etype
(* /from genphp *)

let is_dynamic t = match follow t with
    | TMono _ | TDynamic _
    | TInst({ cl_kind = KTypeParameter _ }, _) -> true
    | TAnon anon ->
        (match !(anon.a_status) with
         | EnumStatics _ | Statics _ -> false
         | _ -> true
        )
    | _ -> false

let is_dynamic_expr e = is_dynamic e.etype

let is_structural_type t =
    match follow t with
    | TDynamic _ -> true
    | TAnon a -> true
    | TType ({t_type = TAnon _},_) -> true
    | _ -> false

let rec is_int_type ctx t =
    match follow t with
    | TInst ({cl_path = ([], "Int")}, _) -> true
    | TAnon a ->
        (match !(a.a_status) with
         | Statics ({cl_path = ([], "Int")}) -> true
         | _ -> false)
    | TAbstract ({a_path = ([],"Float")}, pl) -> false
    | TAbstract ({a_path = ([],"Int")}, pl) -> true
    | TAbstract (a,pl) -> is_int_type ctx (Abstract.get_underlying_type a pl)
    | _ -> false

let rec extract_expr e = match e.eexpr with
    | TParenthesis e
    | TMeta (_,e)
    | TCast(e,_) -> extract_expr e
    | _ -> e

let gen_constant ctx p = function
    | TInt i -> print ctx "%ld" i
    | TFloat s -> spr ctx (replace_float_separators s)
    | TString s -> begin
            add_feature ctx "use.string";
            print ctx "\"%s\"" (s_escape_lua s)
        end
    | TBool b -> spr ctx (if b then "true" else "false")
    | TNull -> spr ctx "nil"
    | TThis -> spr ctx (this ctx)
    | TSuper -> Globals.die "" __LOC__



let rec is_function_type t =
    match follow(t) with
    | TFun _ -> true
    | TAbstract({a_path=["haxe"],"Function" },_) -> true
    | _ -> false

and gen_argument ?(reflect=false) ctx e = begin
    match e.eexpr with
    | TField (x, ((FInstance (_, _, f) | FAnon(f) | FClosure(_,f)) as i)) when ((is_function_type e.etype) && (not(is_dot_access x i))) ->
            (
            if reflect then (
              add_feature ctx "use._hx_funcToField";
              spr ctx "_hx_funcToField(";
            );

            add_feature ctx "use._hx_bind";
            print ctx "_hx_bind(";
            gen_value ctx x;
            print ctx ",";
            gen_value ctx x;
            print ctx "%s)" (if Meta.has Meta.SelfCall f.cf_meta then "" else (field f.cf_name));

            if reflect then
                print ctx ")";
            );
    | _ ->
        gen_value ctx e;
end

and gen_paren_arguments ctx el = begin
    spr ctx "(";
    concat ctx ", " (gen_argument ctx) el;
    spr ctx ")";
end

and gen_call ctx e el =
    ctx.iife_assign <- true;
    (match e.eexpr , el with
     | TConst TSuper , params ->
         (match ctx.current.cl_super with
          | None -> typing_error "Missing api.setCurrentClass" e.epos
          | Some (c,_) ->
              print ctx "%s.super(%s" (ctx.type_accessor (TClassDecl c)) (this ctx);
              List.iter (fun p -> print ctx ","; gen_argument ctx p) params;
              spr ctx ")";
         );
     | TField ({ eexpr = TConst TSuper },f) , params ->
         (match ctx.current.cl_super with
          | None -> typing_error "Missing api.setCurrentClass" e.epos
          | Some (c,_) ->
              let name = field_name f in
              print ctx "%s.prototype%s(%s" (ctx.type_accessor (TClassDecl c)) (field name) (this ctx);
              List.iter (fun p -> print ctx ","; gen_argument ctx p) params;
              spr ctx ")";
         );
     | TField (_, FStatic( { cl_path = ([],"Reflect") }, { cf_name = "callMethod" })), (obj :: fld :: args :: rest) ->
         gen_expr ctx e;
         spr ctx "(";
         gen_argument ctx obj;
         spr ctx ",";
         gen_argument ~reflect:true ctx fld;
         spr ctx ",";
         gen_argument ctx args;
         spr ctx ")";
     | TCall (x,_) , el when (match x.eexpr with TIdent "__lua__" -> false | _ -> true) ->
         gen_paren ctx [e];
         gen_paren_arguments ctx el;
     | TIdent "__new__", { eexpr = TConst (TString cl) } :: params ->
         print ctx "%s.new" cl;
         gen_paren_arguments ctx params;
     | TIdent "__new__", e :: params ->
         gen_value ctx e;
         spr ctx ".new";
         gen_paren_arguments ctx params;
     | TIdent "__callself__", { eexpr = TConst (TString head) } :: { eexpr = TConst (TString tail) } :: el ->
         print ctx "%s:%s" head tail;
         gen_paren_arguments ctx el;
     | TIdent "__call__", { eexpr = TConst (TString code) } :: el ->
         spr ctx code;
         gen_paren_arguments ctx el;
     | TIdent "__lua_length__", [e]->
         spr ctx "#"; gen_value ctx e;
     | TField (_, FStatic ({ cl_path = (["_G"],"table")}, { cf_name = "create" })), el
     | TIdent "__lua_table__", el ->
         let count = ref 0 in
         spr ctx "({";
         List.iter (fun e ->
             (match Texpr.skip e with
              | { eexpr = TArrayDecl arr } ->
                  if (!count > 0 && List.length(arr) > 0) then spr ctx ",";
                  concat ctx "," (gen_value ctx) arr;
                  if List.length(arr) > 0 then incr count;
              | { eexpr = TObjectDecl fields } ->
                  if (!count > 0 && List.length(fields) > 0) then spr ctx ",";
                  concat ctx ", " (fun ((f,_,_),e) ->
                      print ctx "%s = " (anon_field f);
                      gen_value ctx e
                  ) fields;
                  if List.length(fields) > 0 then incr count;
              | { eexpr = TConst(TNull)} -> ()
              | _ ->
				typing_error "__lua_table__ only accepts array or anonymous object arguments" e.epos;
             )) el;
         spr ctx "})";
     | TIdent "__lua__", [{ eexpr = TConst (TString code) }] ->
         spr ctx (String.concat "\n" (ExtString.String.nsplit code "\r\n"))
     | TIdent "__lua__", { eexpr = TConst (TString code); epos = p } :: tl ->
         Codegen.interpolate_code ctx.com code tl (spr ctx) (gen_expr ctx) p
     | TIdent "__type__",  [o] ->
         spr ctx "type";
         gen_paren ctx [o];
     | TIdent "__define_feature__", [_;e] ->
         gen_expr ctx e
     | TIdent "__feature__", { eexpr = TConst (TString f) } :: eif :: eelse ->
         (if has_feature ctx f then
              gen_value ctx eif
          else match eelse with
              | [] -> ()
              | e :: _ -> gen_value ctx e)
     | TIdent "__resources__", [] ->
         (* TODO: Array declaration helper *)
         spr ctx "_hx_tab_array({";
         let count = ref 0 in
         concat ctx "," (fun (name,data) ->
             if (!count == 0) then spr ctx "[0]=";
             spr ctx "{ ";
             spr ctx "name = ";
             gen_constant ctx e.epos (TString name);
             spr ctx ", data = ";
             gen_constant ctx e.epos (TString (Codegen.bytes_serialize data));
             spr ctx "}";
             incr count
         ) (Hashtbl.fold (fun name data acc -> (name,data) :: acc) ctx.com.resources []);
         print ctx "}, %i)" !count;
     | TIdent "`trace", [e;infos] ->
         if has_feature ctx "haxe.Log.trace" then begin
             let t = (try List.find (fun t -> t_path t = (["haxe"],"Log")) ctx.com.types with _ -> Globals.die "" __LOC__) in
             spr ctx (ctx.type_accessor t);
             spr ctx ".trace(";
             gen_value ctx e;
             spr ctx ",";
             gen_value ctx infos;
             spr ctx ")";
         end else begin
             spr ctx "_hx_print(";
             gen_value ctx e;
             spr ctx ")";
         end
     | TField ( { eexpr = TConst(TInt _ | TFloat _| TString _| TBool _) } as e , ((FInstance _ | FAnon _) as ef)), el ->
         gen_paren ctx [e];
         print ctx (":%s(") (field_name ef);
         concat ctx "," (gen_value ctx) el;
         spr ctx ")";
     | TField (_, FStatic( { cl_path = ([],"Std") }, { cf_name = "string" })),[{eexpr = TCall({eexpr=TField (_, FStatic( { cl_path = ([],"Std") }, { cf_name = "string" }))}, _)} as el] ->
         (* unwrap recursive Std.string(Std.string(...)) declarations to Std.string(...) *)
         gen_value ctx el;
     | TField ({eexpr = TLocal _} as e, ef), el when is_possible_string_field e (field_name ef)  ->
         add_feature ctx "use._hx_wrap_if_string_field";
         add_feature ctx "use.string";
         spr ctx "_hx_wrap_if_string_field(";
         gen_value ctx e;
         print ctx ",'%s'" (field_name ef);
         spr ctx ")(";
         concat ctx "," (gen_argument ctx) (e::el);
         spr ctx ")";
     | TField (field_owner, ((FInstance _ | FAnon _ | FDynamic _) as ef)), el ->
         let s = (field_name ef) in
         if Hashtbl.mem kwds s || not (valid_lua_ident s) then begin
             add_feature ctx "use._hx_apply_self";
             spr ctx "_hx_apply_self(";
             gen_value ctx field_owner;
             print ctx ",\"%s\"" (field_name ef);
             if List.length(el) > 0 then spr ctx ",";
             concat ctx "," (gen_argument ctx) el;
             spr ctx ")";
         end else begin
             let el =
                if (match ef with FAnon _ | FDynamic _ -> true | _ -> false) && is_possible_string_field field_owner s then
                    begin
                        gen_expr ctx e;
                        field_owner :: el
                    end
                else
                    begin
                        gen_value ctx field_owner;
                        if is_dot_access field_owner ef then
                            print ctx ".%s" (field_name ef)
                        else
                            print ctx ":%s" (field_name ef);
                        el
                    end
             in
             gen_paren_arguments ctx el;
         end;
     | _ ->
         gen_value ctx e;
         gen_paren_arguments ctx el);
    ctx.iife_assign <- false;

and has_continue e =
    let rec loop e = match e.eexpr with
        | TContinue -> raise Exit
        | TWhile(e1,_,_) | TFor(_,e1,_) -> loop e1 (* in theory there could be a continue there. Note that we don't want to recurse into the loop body because we do not care about inner continue expressions *)
        | _ -> Type.iter loop e
    in
    try
        loop e;
        false;
    with Exit ->
        true

and gen_paren ctx el =
    spr ctx "(";
    concat ctx ", " (gen_value ctx) el;
    spr ctx ")";

and gen_cond ctx cond =
    ctx.iife_assign <- true;
    gen_value ctx cond;
    ctx.iife_assign <- false

and gen_loop ctx label cond e =
    let old_in_loop = ctx.in_loop in
    ctx.in_loop <- true;
    let old_handle_continue = ctx.handle_continue in
    let will_continue = has_continue e in
    ctx.handle_continue <- has_continue e;
    ctx.break_depth <- ctx.break_depth + 1;
    if will_continue then begin
        println ctx "local _hx_continue_%i = false;" ctx.break_depth;
    end;
    let b = open_block ctx in
    print ctx "%s " label;
    gen_cond ctx cond;
    print ctx " do ";
    if will_continue then print ctx "repeat ";
    gen_block_element ctx e;
    if will_continue then begin
        if will_continue then begin
            println ctx "until true";
        end;
        println ctx "if _hx_continue_%i then " ctx.break_depth;
        println ctx "_hx_continue_%i = false;" ctx.break_depth;
        if ctx.in_loop_try then
            println ctx "_G.error(\"_hx_pcall_break\");"
        else
            println ctx "break;";
        println ctx "end;";
    end;
    b();
    newline ctx;
    print ctx "end";
    ctx.in_loop <- old_in_loop;
    ctx.break_depth <- ctx.break_depth-1;
    ctx.handle_continue <- old_handle_continue;


and is_possible_string_field e field_name=
    (* Special case for String fields *)
    let structural_type = is_structural_type e.etype in
    if not structural_type then
        false
    else match field_name with
        | "length"
        | "toUpperCase"
        | "toLowerCase"
        | "charAt"
        | "indexOf"
        | "lastIndexOf"
        | "split"
        | "toString"
        | "substring"
        | "substr"
        | "charCodeAt" ->
            true
        | _ ->
            false


and ttype_multireturn t = match t with
    | TInst (c,_) ->
            Meta.has Meta.MultiReturn c.cl_meta
    | TType (c,_) ->
            Meta.has Meta.MultiReturn c.t_meta
    | _ ->
            false
and check_multireturn_param ctx t pos =
   match t with
         TAbstract(_,p) | TInst(_,p) ->
            if List.exists ttype_multireturn p then
				 typing_error "MultiReturns must not be type parameters" pos
            else
                ()
        | _ ->
                ();

(* for declaring identifiers in values/blocks *)
and lua_ident_name a =
    match a.v_name, a.v_kind, a.v_type with
        | "this", _, _ -> "self";
        | _, _, _ ->  ident a.v_name;


(* for declaring arguments in function defintions *)
and lua_arg_name(a,_) =
    match a.v_name, a.v_kind, a.v_type with
        | "this", _, _ -> "self";
        | _, _, TAbstract({a_path=["haxe"],"Rest" },_) -> "...";
        | _, _, _ ->  ident a.v_name;

and gen_expr ?(local=true) ctx e = begin
    match e.eexpr with
      TConst c ->
        gen_constant ctx e.epos c;
    | TLocal v -> spr ctx (lua_ident_name v);
    | TArray (e1,{ eexpr = TConst (TString s) }) when valid_lua_ident s && (match e1.eexpr with TConst (TInt _|TFloat _) -> false | _ -> true) ->
        gen_value ctx e1;
        spr ctx (field s)
    | TArray (e1,e2) ->
        gen_value ctx e1;
        spr ctx "[";
        gen_value ctx e2;
        spr ctx "]";
    | TBinop (op,e1,e2) ->
        gen_tbinop ctx op e1 e2;
    | TField (x,FClosure (_,f)) ->
        add_feature ctx "use._hx_bind";
        (match x.eexpr with
         | TConst _ | TLocal _ ->
             print ctx "_hx_bind(";
             gen_value ctx x;
             print ctx ",";
             gen_value ctx x;
             print ctx "%s)" (if Meta.has Meta.SelfCall f.cf_meta then "" else (field f.cf_name))
         | _ ->
             print ctx "(function() local __=";
             gen_value ctx x;
             print ctx "; return _hx_bind(__,__%s) end)()" (if Meta.has Meta.SelfCall f.cf_meta then "" else (field f.cf_name)))
    | TEnumParameter (x,_,i) ->
        gen_value ctx x;
        print ctx "[%i]" (i + 2)
    | TEnumIndex x ->
        gen_value ctx x;
        print ctx "[1]"
    | TField (e, ef) when is_string_expr e && field_name ef = "length" ->
        if ctx.lua_vanilla then (
            spr ctx "#";
            gen_value ctx e;
        ) else (
            spr ctx "__lua_lib_luautf8_Utf8.len(";
            gen_value ctx e;
            spr ctx ")";
        )
    | TField (e, ef) when is_possible_string_field e (field_name ef)  ->
        add_feature ctx "use._hx_wrap_if_string_field";
        add_feature ctx "use.string";
        spr ctx "_hx_wrap_if_string_field(";
        gen_value ctx e;
        print ctx ",'%s')" (field_name ef)
    | TField (x, (FInstance(_,_,f) | FStatic(_,f) | FAnon(f))) when Meta.has Meta.SelfCall f.cf_meta ->
        gen_value ctx x;
    | TField ({ eexpr = TConst(TInt _ | TFloat _| TString _| TBool _) } as e , ((FInstance _ | FAnon _) as ef)) ->
        gen_paren ctx [e];
        print ctx (".%s") (field_name ef);
    | TField ({ eexpr = TConst (TInt _ | TFloat _) } as x,f) ->
        gen_expr ctx { e with eexpr = TField(mk (TParenthesis x) x.etype x.epos,f) }
    | TField ({ eexpr = TObjectDecl fields }, ef ) ->
        spr ctx "(function(x) return x.";
        print ctx "%s" (field_name ef);
        spr ctx " end )({";
        concat ctx ", " (fun ((f,_,_),e) -> print ctx "%s = " (anon_field f); gen_value ctx e) fields;
        spr ctx "})";
    | TField ({eexpr = TLocal v}, f) when Meta.has Meta.MultiReturn v.v_meta ->
        (* field of a multireturn local var is actually just a local var *)
        let (_, args, pos) =  Meta.get (Meta.Custom ":lua_mr_id") v.v_meta  in
        (match args with
         | [(EConst(String(id,_)), _)] ->
             spr ctx (id ^ "_" ^ (ident v.v_name) ^ "_" ^ (field_name f));
         | _ ->
             Globals.die "" __LOC__);
    | TField (_, (FStatic ({cl_path = [],""},_) as f)) ->
        spr ctx (ident (field_name f))
    | TField (x,f) ->
        gen_value ctx x;
        let name = field_name f in
        spr ctx (match f with FStatic _ | FEnum _ | FInstance _ | FAnon _ | FDynamic _ | FClosure _ -> field name)
    | TTypeExpr t ->
        spr ctx (ctx.type_accessor t)
    | TParenthesis e ->
        gen_paren ctx [e];
    | TMeta (_,e) ->
        gen_expr ctx e
    | TReturn eo -> gen_return ctx e eo;
    | TBreak ->
        if not ctx.in_loop then unsupported e.epos;
        if ctx.handle_continue then
            print ctx "_hx_continue_%i = true;" ctx.break_depth;
        if ctx.in_loop_try then
            print ctx "_G.error(\"_hx_pcall_break\", 0)"
        else
            spr ctx "break"
    | TContinue ->
        if not ctx.in_loop then unsupported e.epos;
        if ctx.in_loop_try then
            print ctx "_G.error(\"_hx_pcall_break\", 0)"
        else
            spr ctx "break" (*todo*)
    | TBlock el ->
        let bend = open_block ctx in
        List.iter (gen_block_element ctx) el;
        bend();
        newline ctx;
    | TFunction f ->
        let old = ctx.in_value, ctx.in_loop in
        ctx.in_value <- None;
        ctx.in_loop <- false;
        print ctx "function(%s) " (String.concat "," (List.map lua_arg_name f.tf_args));
        let fblock = fun_block ctx f e.epos in
        (match fblock.eexpr with
         | TBlock el ->
             let bend = open_block ctx in
             List.iter (gen_block_element ctx) el;
             bend();
             newline ctx;
         |_ -> ());
        spr ctx "end";
        ctx.in_value <- fst old;
        ctx.in_loop <- snd old;
        ctx.separator <- true
    | TCall (e,el) ->
        gen_call ctx e el;
    | TArrayDecl el ->
        spr ctx "_hx_tab_array({";
        let count = ref 0 in
        List.iteri (fun i e ->
            incr count;
            if (i == 0) then spr ctx "[0]="
            else spr ctx ", ";
            gen_value ctx e) el;
        print ctx "}, %i)" !count;
    | TThrow e ->
        spr ctx "_G.error(";
        gen_value ctx e;
        spr ctx ",0)";
    | TVar (v,eo) ->
        begin match eo with
            | None ->
                if local then
                    spr ctx "local ";
                spr ctx (ident v.v_name);
            | Some e ->
                match e.eexpr with
                | TBinop(OpAssign, e1, e2) ->
                    gen_tbinop ctx OpAssign e1 e2;
                    if local then
                        spr ctx " local ";
                    spr ctx (ident v.v_name);
                    spr ctx " = ";
                    gen_value ctx e1;

                | _ when Meta.has Meta.MultiReturn v.v_meta ->
                    (* multi-return var is generated as several vars for unpacking *)
                    let id = temp ctx in
                    let temp_expr = (EConst(String(id,SDoubleQuotes)), Globals.null_pos) in
                    v.v_meta <- (Meta.Custom ":lua_mr_id", [temp_expr], v.v_pos) :: v.v_meta;
                    let name = ident v.v_name in
                    let names =
                        match follow v.v_type with
                        | TInst (c, _) ->
                            List.map (fun f -> id ^ "_" ^name ^ "_" ^ f.cf_name) c.cl_ordered_fields
                        | _ ->
                            Globals.die "" __LOC__
                    in
                    spr ctx "local ";
                    spr ctx (String.concat ", " names);
                    spr ctx " = ";
                    gen_value ctx e;

                | _ ->
                    if local then
                        spr ctx "local ";
                    spr ctx (ident v.v_name);
                    spr ctx " = ";

                    (* if it was a multi-return var but it was used as a value itself, *)
                    (* we have to box it in an object conforming to a multi-return extern class *)
                    let is_boxed_multireturn = Meta.has (Meta.Custom ":lua_mr_box") v.v_meta in
                    let e = if is_boxed_multireturn then mk_mr_box ctx e else e in
                    (match e.eexpr with
                    | TCast ({ eexpr = TTypeExpr mt } as e1, None) when (match mt with TClassDecl {cl_path = ([],"Array")} -> false | _ -> true) ->
                        add_feature ctx "use._hx_staticToInstance";
                        spr ctx "_hx_staticToInstance(";
                        gen_expr ctx e1;
                        spr ctx ")";
                    | _ -> gen_value ctx e);
        end
    | TNew (c,_,el) ->
        (match c.cl_constructor with
         | Some cf when Meta.has Meta.SelfCall cf.cf_meta ->
             print ctx "%s" (ctx.type_accessor (TClassDecl c));
         | Some cf when Meta.has Meta.Native cf.cf_meta ->
             let _, args, mp = Meta.get Meta.Native cf.cf_meta in
             (match args with
              | [( EConst(String(s,_)),_)] ->
                  print ctx "%s.%s" (ctx.type_accessor (TClassDecl c)) s;
              | _ ->
                  print ctx "%s.new" (ctx.type_accessor (TClassDecl c)));
         | _ -> print ctx "%s.new" (ctx.type_accessor (TClassDecl c)));
        gen_paren ctx el;
    | TIf (cond,e,eelse) ->
        ctx.iife_assign <- true;
        spr ctx "if ";
        gen_cond ctx cond;
        spr ctx " then ";
        let bend = open_block ctx in
        gen_block_element ctx e;
        bend();
        newline ctx;
        (match eelse with
         | None -> ();
         | Some e2 ->
             (match e.eexpr with
              | TObjectDecl _ -> ctx.separator <- false
              | _ ->());
             spr ctx "else";
             let bend = open_block ctx in
             gen_block_element ctx e2;
             bend();
             newline ctx);
        spr ctx "end";
        ctx.iife_assign <- false;
    | TUnop ((Increment|Decrement) as op,unop_flag, e) ->
        (* TODO: Refactor this mess *)
        println ctx "(function() ";
        (match e.eexpr, unop_flag with
         | TArray(e1,e2), _ ->
             spr ctx "local _hx_idx = "; gen_value ctx e2; semicolon ctx; newline ctx;
             spr ctx "local _hx_arr ="; gen_value ctx e1; semicolon ctx; newline ctx;
             (match unop_flag with
              | Ast.Postfix ->
                  spr ctx "local _ = _hx_arr[_hx_idx]"; semicolon ctx; newline ctx;
              | _ -> ());
             spr ctx "_hx_arr[_hx_idx] = _hx_arr[_hx_idx]";
         | TField(e1,e2), _ ->
             spr ctx "local _hx_obj = "; gen_value ctx e1; semicolon ctx; newline ctx;
             spr ctx "local _hx_fld = ";
             (match e2 with
              | FInstance(_,_,fld)
              | FStatic(_,fld)
              | FAnon fld
              | FClosure(_,fld) ->
                  print ctx "'%s'" fld.cf_name;
              | FDynamic name ->
                  print ctx "'%s'" name;
              | FEnum(_,efld) ->
                  print ctx "'%s'" efld.ef_name);
             semicolon ctx; newline ctx;
             (match unop_flag with
              | Ast.Postfix ->
                  spr ctx "local _ = _hx_obj[_hx_fld]"; semicolon ctx; newline ctx;
              | _ -> ());
             spr ctx "_hx_obj[_hx_fld] = _hx_obj[_hx_fld] ";
         | _, Ast.Prefix ->
             gen_value ctx e;
             spr ctx " = ";
             gen_value ctx e;
         | _, Ast.Postfix ->
             spr ctx "local _ = ";
             gen_value ctx e; semicolon ctx;
             gen_value ctx e;
             spr ctx " = ";
             gen_value ctx e);
        (match op with
         |Increment -> spr ctx " + 1;"
         |Decrement -> spr ctx " - 1;"
         |_-> print ctx " %s 1;" (Ast.s_unop op));
        newline ctx;
        spr ctx " return ";
        (match unop_flag, e.eexpr with
         | Ast.Postfix, _ ->
             spr ctx "_";
         | _, TArray(e1,e2) ->
             spr ctx "_hx_arr[_hx_idx]";
         | _, TField(e1,e2) ->
             spr ctx "_hx_obj[_hx_fld]";
         | _, _ ->
             gen_value ctx e;
        );
        semicolon ctx; newline ctx;
        spr ctx " end)()";
    | TUnop (Not,unop_flag,e) ->
        spr ctx "not ";
        gen_value ctx e;
    | TUnop (NegBits,unop_flag,e) ->
        add_feature ctx "use._bitop";
        spr ctx "_hx_bit.bnot(";
        gen_value ctx e;
        spr ctx ")";
    | TUnop (Spread,Prefix,e) ->
        add_feature ctx "use._hx_table";
        spr ctx "_hx_table.unpack(";
        gen_value ctx e;
        spr ctx ")";
    | TUnop (op,Ast.Prefix,e) ->
        spr ctx (Ast.s_unop op);
        gen_value ctx e
    | TUnop (op,Ast.Postfix,e) ->
        gen_value ctx e;
        spr ctx (Ast.s_unop op)
    | TWhile (cond,e,Ast.NormalWhile) ->
        gen_loop ctx "while" cond e
    | TWhile (cond,e,Ast.DoWhile) ->
        gen_block_element ctx e;
        newline ctx;
        gen_loop ctx "while" cond e
    | TObjectDecl [] ->
        spr ctx "_hx_e()";
        ctx.separator <- true
    | TObjectDecl fields ->
        spr ctx "_hx_o({__fields__={";
        concat ctx "," (fun ((f,_,_),e) -> print ctx "%s=" (anon_field f); spr ctx "true") fields;
        spr ctx "},";
        concat ctx "," (fun ((f,_,_),e) -> print ctx "%s=" (anon_field f); gen_anon_value ctx e) fields;
        spr ctx "})";
        ctx.separator <- true
    | TFor (v,it,e2) ->
        unsupported e.epos;
    | TTry (e,catchs) ->
        (* TODO: add temp variables *)
        let old_in_loop_try = ctx.in_loop_try in
        if ctx.in_loop then
            ctx.in_loop_try <- true;
        println ctx "local _hx_status, _hx_result = pcall(function() ";
        let b = open_block ctx in
        gen_expr ctx e;
        b();
        println ctx "return _hx_pcall_default";
        println ctx "end)";
        ctx.in_loop_try <- old_in_loop_try;
        println ctx "if not _hx_status and _hx_result == \"_hx_pcall_break\" then";
        if ctx.in_loop then
            if old_in_loop_try then
                println ctx "  _G.error(_hx_result,0);"
            else
                println ctx "  break";
        println ctx "elseif not _hx_status then ";
        let bend = open_block ctx in
        (match catchs with
        | [v,e] ->
            print ctx "  local %s = _hx_result;" v.v_name;
            gen_block_element ctx e;
        | _ -> Globals.die "" __LOC__
        );
        bend();
        newline ctx;
        println ctx "elseif _hx_result ~= _hx_pcall_default then";
        println ctx "  return _hx_result";
        print ctx "end";
    | TSwitch (e,cases,def) ->
        List.iteri (fun cnt (el,e2) ->
            if cnt == 0 then spr ctx "if "
            else (newline ctx; spr ctx "elseif ");
            List.iteri (fun ccnt e3 ->
                if ccnt > 0 then spr ctx " or ";
                gen_value ctx e;
                spr ctx " == ";
                gen_value ctx e3;
            ) el;
            print ctx " then ";
            let bend = open_block ctx in
            gen_block_element ctx e2;
            bend();
        ) cases;
        (match def with
         | None -> spr ctx " end"
         | Some e ->
             begin
                 if (List.length(cases) > 0) then
                     spr ctx "else";
                 let bend = open_block ctx in
                 bend();
                 gen_block_element ctx e;
                 if (List.length(cases) > 0) then
                     spr ctx " end";
             end;);
    | TCast (e1,Some t) ->
        print ctx "%s.__cast(" (ctx.type_accessor (TClassDecl { null_class with cl_path = ["lua"],"Boot" }));
        gen_expr ctx e1;
        spr ctx " , ";
        spr ctx (ctx.type_accessor t);
        spr ctx ")"
    | TCast (e1,None) ->
        gen_value ctx e1;
    | TIdent s ->
        spr ctx s
end;

    (* gen_block_element handles expressions that map to "statements" in lua. *)
    (* It handles no-op situations, and ensures that expressions are formatted with newlines *)
and gen_block_element ctx e  =
    ctx.iife_assign <- false;
    begin match e.eexpr with
        | TTypeExpr _ | TConst _ | TLocal _ | TFunction _ ->
            ()
        | TCast (e1, Some t)->
            print ctx "%s.__cast(" (ctx.type_accessor (TClassDecl { null_class with cl_path = ["lua"],"Boot" }));
            gen_expr ctx e1;
            spr ctx " , ";
            spr ctx (ctx.type_accessor t);
            spr ctx ")"
        | TCast (e', None) | TParenthesis e' | TMeta (_,e') ->
            gen_block_element ctx e'
        | TArray (e1,e2) ->
            gen_block_element ctx e1;
            gen_block_element ctx e2;
        | TArrayDecl el | TBlock el ->
            List.iter (gen_block_element ctx) el;
            (* For plain lua table instantiations, just capture argument operations *)
        | TCall({ eexpr = TIdent "__lua_table__"} , el) ->
            List.iter(fun x -> gen_block_element ctx x) el
        (* make a no-op __define_feature__ expression possible *)
        | TCall({eexpr = TIdent "__define_feature__"}, [_;e]) ->
            gen_block_element ctx e
        | TObjectDecl fl ->
            List.iter (fun (_,e) -> gen_block_element ctx e) fl
        | TBinop (op,e1,e2) when op <> Ast.OpAssign ->
            newline ctx;
            let f () = gen_tbinop ctx op e1 e2 in
            gen_iife_assign ctx f;
        | TUnop ((Increment|Decrement) as op,_,e) ->
            newline ctx;
            gen_expr ctx e;
            print ctx " = ";
            gen_expr ctx e;
            (match op with
             | Increment -> print ctx " + 1;"
             | _ -> print ctx " - 1;"
            )
        | TSwitch (e,[],def) ->
            (match def with
             | None -> ()
             | Some e -> gen_block_element ctx e)
        | TField _ ->
            newline ctx;
            let f () = gen_expr ctx e in
            gen_iife_assign ctx f;
            semicolon ctx;
        | TCall ({ eexpr = TIdent "__feature__" }, { eexpr = TConst (TString f) } :: eif :: eelse) ->
            if has_feature ctx f then
                gen_block_element ctx eif
            else (match eelse with
                | [] -> ()
                | [e] -> gen_block_element ctx e
                | _ -> Globals.die "" __LOC__)
        | _ ->
            newline ctx;
            gen_expr ctx e;
            semicolon ctx;
    end;

and is_const_null e =
    match e.eexpr with
    | TConst TNull ->
        true
    | _ ->
        false

    (* values generated in anon structures can get modified.  Functions are bind-ed *)
    (* and include a dummy "self" leading variable so they can be called like normal *)
    (* instance methods *)
and gen_anon_value ctx e =
    match e with
    | { eexpr = TFunction f} ->
        let old = ctx.in_value, ctx.in_loop in
        ctx.in_value <- None;
        ctx.in_loop <- false;
        print ctx "function(%s) " (String.concat "," ("self" :: (List.map lua_arg_name f.tf_args)));
        let fblock = fun_block ctx f e.epos in
        (match fblock.eexpr with