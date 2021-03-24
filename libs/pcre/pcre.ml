(*
   PCRE-OCAML - Perl Compatibility Regular Expressions for OCaml
   Copyright (C) 1999-  Markus Mottl
   email: markus.mottl@gmail.com
   WWW:   http://www.ocaml.info
   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.
   This library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.
   You should have received a copy of the GNU Lesser General Public
   License along with this library; if not, write to the Free Software
   Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
*)

(* Public exceptions and their registration with the C runtime *)

let string_copy str = str
let buffer_add_subbytes = Buffer.add_subbytes

type error =
  | Partial
  | BadPartial
  | BadPattern of string * int
  | BadUTF8
  | BadUTF8Offset
  | MatchLimit
  | RecursionLimit
  | InternalError of string

exception Error of error
exception Backtrack
exception Regexp_or of string * error

(* Puts exceptions into global C-variables for fast retrieval *)
external pcre_ocaml_init : unit -> unit = "pcre_ocaml_init"

(* Registers exceptions with the C runtime and caches polymorphic variants *)
let () =
  Callback.register_exception "Pcre.Error" (Error (InternalError ""));
  Callback.register_exception "Pcre.Backtrack" Backtrack;
  pcre_ocaml_init ()


(* Compilation and runtime flags and their conversion functions *)

type icflag = int
type irflag = int

(* Compilation flags *)

type cflag =
  [
  | `CASELESS
  | `MULTILINE
  | `DOTALL
  | `EXTENDED
  | `ANCHORED
  | `DOLLAR_ENDONLY
  | `EXTRA
  | `UNGREEDY
  | `UTF8
  | `NO_UTF8_CHECK
  | `NO_AUTO_CAPTURE
  | `AUTO_CALLOUT
  | `FIRSTLINE
  | `UCP
  ]

let int_of_cflag = function
  | `CASELESS -> 0x0001
  | `MULTILINE -> 0x0002
  | `DOTALL -> 0x0004
  | `EXTENDED -> 0x0008
  | `ANCHORED -> 0x0010
  | `DOLLAR_ENDONLY -> 0x0020
  | `EXTRA -> 0x0040
  | `UNGREEDY -> 0x0200
  | `UTF8 -> 0x0800
  | `NO_AUTO_CAPTURE -> 0x1000
  | `NO_UTF8_CHECK -> 0x2000
  | `AUTO_CALLOUT -> 0x4000
  | `FIRSTLINE -> 0x40000
  | `UCP -> 0x20000000

let coll_icflag icflag flag = int_of_cflag flag lor icflag
let cflags flags = List.fold_left coll_icflag 0 flags

let cflag_of_int = function
  | 0x0001 -> `CASELESS
  | 0x0002 -> `MULTILINE
  | 0x0004 -> `DOTALL
  | 0x0008 -> `EXTENDED
  | 0x0010 -> `ANCHORED
  | 0x0020 -> `DOLLAR_ENDONLY
  | 0x0040 -> `EXTRA
  | 0x0200 -> `UNGREEDY
  | 0x0800 -> `UTF8
  | 0x1000 -> `NO_AUTO_CAPTURE
  | 0x2000 -> `NO_UTF8_CHECK
  | 0x4000 -> `AUTO_CALLOUT
  | 0x40000 -> `FIRSTLINE
  | 0x20000000 -> `UCP
  | _ -> failwith "Pcre.cflag_list: unknown compilation flag"

let all_cflags =
  [
    0x0001; 0x0002; 0x0004; 0x0008; 0x0010; 0x0020;
    0x0040; 0x0200; 0x0800; 0x1000; 0x2000; 0x4000; 0x40000;
	0x20000000
  ]

let cflag_list icflags =
  let coll flag_list flag =
    if icflags land flag <> 0 then cflag_of_int flag :: flag_list
    else flag_list in
  List.fold_left coll [] all_cflags


(* Runtime flags *)

type rflag =
  [
  | `ANCHORED
  | `NOTBOL
  | `NOTEOL
  | `NOTEMPTY
  | `PARTIAL
  ]

let int_of_rflag = function
  | `ANCHORED -> 0x0010
  | `NOTBOL -> 0x0080
  | `NOTEOL -> 0x0100
  | `NOTEMPTY -> 0x0400
  | `PARTIAL -> 0x8000

let coll_irflag irflag flag = int_of_rflag flag lor irflag
let rflags flags = List.fold_left coll_irflag 0 flags

let rflag_of_int = function
  | 0x0010 -> `ANCHORED
  | 0x0080 -> `NOTBOL
  | 0x0100 -> `NOTEOL
  | 0x0400 -> `NOTEMPTY
  | 0x8000 -> `PARTIAL
  | _ -> failwith "Pcre.rflag_list: unknown runtime flag"

let all_rflags = [0x0010; 0x0080; 0x0100; 0x0400; 0x8000]

let rflag_list irflags =
  let coll flag_list flag =
    if irflags land flag <> 0 then rflag_of_int flag :: flag_list
    else flag_list in
  List.fold_left coll [] all_rflags


(* Information on the PCRE-configuration (build-time options) *)

external pcre_version : unit -> string = "pcre_version_stub"

external pcre_config_utf8 : unit -> bool = "pcre_config_utf8_stub" [@@noalloc]

external pcre_config_newline :
  unit -> char = "pcre_config_newline_stub" [@@noalloc]

external pcre_config_link_size :
  unit -> int = "pcre_config_link_size_stub" [@@noalloc]

external pcre_config_match_limit :
  unit -> int = "pcre_config_match_limit_stub" [@@noalloc]

external pcre_config_match_limit_recursion :
  unit -> int = "pcre_config_match_limit_recursion_stub" [@@noalloc]

external pcre_config_stackrecurse :
  unit -> bool = "pcre_config_stackrecurse_stub" [@@noalloc]

let version = pcre_version ()
let config_utf8 = pcre_config_utf8 ()
let config_newline = pcre_config_newline ()
let config_link_size = pcre_config_link_size ()
let config_match_limit = pcre_config_match_limit ()
let config_match_limit_recursion = pcre_config_match_limit_recursion ()
let config_stackrecurse = pcre_config_stackrecurse ()


(* Information on patterns *)

type firstbyte_info =
  [ `Char of char
  | `Start_only
  | `ANCHORED ]

type study_stat =
  [ `Not_studied
  | `Studied
  | `Optimal ]

type regexp

external options : regexp -> icflag = "pcre_options_stub"
external size : regexp -> int = "pcre_size_stub"
external studysize : regexp -> int = "pcre_studysize_stub"
external capturecount : regexp -> int = "pcre_capturecount_stub"
external backrefmax : regexp -> int = "pcre_backrefmax_stub"
external namecount : regexp -> int = "pcre_namecount_stub"
external names : regexp -> string array = "pcre_names_stub"
external nameentrysize : regexp -> int = "pcre_nameentrysize_stub"
external firstbyte : regexp -> firstbyte_info = "pcre_firstbyte_stub"
external firsttable : regexp -> string option = "pcre_firsttable_stub"
external lastliteral : regexp -> char option = "pcre_lastliteral_stub"
external study_stat : regexp -> study_stat = "pcre_study_stat_stub" [@@noalloc]


(* Compilation of patterns *)

type chtables

external maketables : unit -> chtables = "pcre_maketables_stub"

(*  Internal use only! *)
external pcre_study : regexp -> unit = "pcre_study_stub"

external compile :
  icflag -> chtables option -> string -> regexp = "pcre_compile_stub"

external get_match_limit : regexp -> int option = "pcre_get_match_limit_stub"

(* Internal use only! *)
external set_imp_match_limit :
  regexp -> int -> regexp = "pcre_set_imp_match_limit_stub" [@@noalloc]

external get_match_limit_recursion :
  regexp -> int option = "pcre_get_match_limit_recursion_stub"

(* Internal use only! *)
external set_imp_match_limit_recursion :
  regexp -> int -> regexp = "pcre_set_imp_match_limit_recursion_stub" [@@noalloc]

let regexp
      ?(study = true) ?limit ?limit_recursion
      ?(iflags = 0) ?flags ?chtables pat =
  let rex =
    match flags with
    | Some flag_list -> compile (cflags flag_list) chtables pat
    | _ -> compile iflags chtables pat
  in
  if study then pcre_study rex;
  let rex =
    match limit with
    | None -> rex
    | Some lim -> set_imp_match_limit rex lim
  in
  match limit_recursion with
  | None -> rex
  | Some lim -> set_imp_match_limit_recursion rex lim

let regexp_or
      ?study ?limit ?limit_recursion ?(iflags = 0) ?flags ?chtables pats =
  let check pat =
    try ignore (regexp ~study:false ~iflags ?flags ?chtables pat)
    with Error error -> raise (Regexp_or (pat, error))
  in
  List.iter check pats;
  let big_pat =
    let cnv pat = "(?:" ^ pat ^ ")" in
    String.concat "|" (List.rev (List.rev_map cnv pats))
  in
  regexp ?study ?limit ?limit_recursion ~iflags ?flags ?chtables big_pat

let bytes_unsafe_blit_string str str_ofs bts bts_ofs len =
  let str_bts = Bytes.unsafe_of_string str in
  Bytes.unsafe_blit str_bts str_ofs bts bts_ofs len

let string_unsafe_sub str ofs len =
  let res = Bytes.create len in
  bytes_unsafe_blit_string str ofs res 0 len;
  Bytes.unsafe_to_string res

let quote s =
  let len = String.length s in
  let buf = Bytes.create (len lsl 1) in
  let pos = ref 0 in
  for i = 0 to len - 1 do
    match String.unsafe_get s i with
    | '\\' | '^' | '$' | '.' | '[' | '|'
    | '('  | ')' | '?' | '*' | '+' | '{' as c ->
      Bytes.unsafe_set buf !pos '\\';
      incr pos;
      Bytes.unsafe_set buf !pos c;
      incr pos
    | c -> Bytes.unsafe_set buf !pos c; incr pos
  done;
  string_unsafe_sub (Bytes.unsafe_to_string buf) 0 !pos


(* Matching of patterns and subpattern extraction *)

(* Default regular expression when none is provided by the user *)
let def_rex = regexp 