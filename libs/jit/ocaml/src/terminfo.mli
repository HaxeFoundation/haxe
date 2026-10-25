(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Basic interface to the terminfo database

  {b Warning:} this module is unstable and part of
  {{!Compiler_libs}compiler-libs}.

*)

type status =
  | Uninitialised
  | Bad_term
  | Good_term

val setup : out_channel -> status
val num_lines : out_channel -> int
val backup : out_channel -> int -> unit
val standout : out_channel -> bool -> unit
val resume : out_channel -> int -> unit
