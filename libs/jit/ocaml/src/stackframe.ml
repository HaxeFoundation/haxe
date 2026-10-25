(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cambium, INRIA Paris                  *)
(*                                                                        *)
(*   Copyright 2023 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Compute the parameters needed for allocating and managing stack frames
   in the Emit phase. *)

open Mach

let trap_handler_size = 16

class stackframe = object

inherit Stackframegen.stackframe_generic as super

method trap_handler_size = trap_handler_size

method! is_call = function
  | Iop (Iintop (Icheckbound) | Iintop_imm(Icheckbound, _)) -> true
  | insn -> super#is_call insn

method! frame_required f contains_calls =
  Config.with_frame_pointers || super#frame_required f contains_calls

end

let analyze f =
  (new stackframe)#analyze f
