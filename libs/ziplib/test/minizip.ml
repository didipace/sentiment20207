(***********************************************************************)
(*                                                                     *)
(*                         The CamlZip library                         *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 2001 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the GNU Library General Public License, with    *)
(*  the special exception on linking described in file LICENSE.        *)
(*                                                                     *)
(***********************************************************************)

(* $Id: minizip.ml,v 1.2 2006/04/04 08:29:07 xleroy Exp $ *)

open Printf

let list_entry e =
  let t = Unix.localtime e.Zip.mtime in
  printf "%6d  %6d  %c  %04d-%02d-%02d %02d:%02d  %c  %s\n"
    e.Zip.uncompressed_size
    e.Zip.compressed_size
    (match e.Zip.methd with Zip.Stored -> 's' | Zip.Deflated -> 'd')
    (t.Unix.tm_year + 1900) (t.Unix.tm_mon + 1) t.Unix.tm_mday
    t.Unix.tm_hour t.Unix.tm_min
    (if e.Zip.is_directory then 'd' else ' ')
    e.Zip.filename;
  if e.Zip.comment <> "" then
    printf "        %s\n" e.Zip.comment

let list zipfil