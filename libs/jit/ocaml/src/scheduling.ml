# 2 "asmcomp/amd64/scheduling.ml"
(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 2000 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* The "open!" directive below is necessary because, although
   this module does not actually depend on Schedgen in this backend, the
   dependency exists in other backends and our build system requires
   that all the backends have the same dependencies.
   We thus have to use "open!" and disable the corresponding warning
   only for this compilation unit.
*)

open! Schedgen [@@warning "-66"]

(* Scheduling is turned off because the processor schedules dynamically
   much better than what we could do. *)

let fundecl f = f
