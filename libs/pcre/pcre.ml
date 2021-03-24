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
  | 0