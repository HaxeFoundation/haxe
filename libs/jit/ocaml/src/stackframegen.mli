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

type analysis_result = {
  contains_nontail_calls: bool;
    (** Whether the function contains non-tail calls to OCaml functions.
        Calls to C functions don't count.
    *)
  frame_required: bool;
    (** Whether the function must allocate a stack frame on entry, for
        one of the following reasons:
        - some local variables are stack-allocated
        - the function needs to save its return address on the stack, e.g.:
           -- it contains a non-tail call to an OCaml function
           -- it calls a C function
           -- it contains an allocation or a poll point
           -- it performs an array bound check (on some ports)
     *)
  extra_stack_used: int;
     (** Amount of stack space used, in bytes,
         in addition to the initial stack frame.
         This counts trap handlers and "outgoing" stack slots used
         for parameter passing. *)
}

class virtual stackframe_generic : object
  method virtual trap_handler_size : int
  method is_call : Mach.instruction_desc -> bool
  method frame_required : Mach.fundecl -> bool -> bool
  method analyze : Mach.fundecl -> analysis_result
end
