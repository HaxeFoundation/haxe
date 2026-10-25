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

type analysis_result = {
  contains_nontail_calls: bool;
  frame_required: bool;
  extra_stack_used: int;
}

class virtual stackframe_generic = object (self)

(* Size of an exception handler block on the stack.
   To be provided for each target. *)

method virtual trap_handler_size : int

(* Determine if an instruction performs a call that requires
   the return address to be saved in the stack frame, and a stack frame to
   be allocated.

   At a minimum, these instructions include all non-tail calls,
   both to OCaml functions or to C functions.

   For exception-raising constructs, we get better stack backtraces
   by treating them as non-tail calls, even if they are implemented as
   tail calls.

   This method can be overridden in [Stackframe] to implement target-specific
   behaviors. *)

method is_call = function
  | Iop (Icall_ind | Icall_imm _ | Iextcall _) -> true
  | Iop (Ialloc _) | Iop (Ipoll _) -> true
      (* caml_alloc*, caml_garbage_collection (incl. polls) *)
  | Iop (Iintop (Icheckbound) | Iintop_imm(Icheckbound, _)) -> !Clflags.debug
      (* caml_ml_array_bound_error *)
  | Iraise Lambda.Raise_notrace -> false
  | Iraise (Lambda.Raise_regular | Lambda.Raise_reraise) -> true
      (* caml_stash_backtrace; having a frame gives better stack backtrace *)
  | Itrywith _ -> true
  | _ -> false

(* Determine if a function requires a stack frame to be allocated.
   This is the case if it contains calls, but also if it allocates
   variables on the stack.

   This method can be overridden in [Stackframe] to implement target-specific
   behaviors. *)

method frame_required f contains_calls =
  contains_calls ||
  f.fun_num_stack_slots.(0) > 0 || f.fun_num_stack_slots.(1) > 0

(* Analyze the body of a Mach function to determine
   - whether it contains non-tail-calls to OCaml functions
   - whether it requires allocating a stack frame and saving the return address
   - how much extra stack space is needed for exception handlers
     and for passing parameters to C function on stack.
*)

method analyze f =
  let contains_nontail_calls = ref false
  and contains_calls = ref false
  and extra_space = ref 0 in
  let rec analyze sp i =
    if sp > !extra_space then extra_space := sp;
    contains_calls := !contains_calls || self#is_call i.desc;
    match i.desc with
    | Iend -> ()
    | Iop (Istackoffset delta) ->
        analyze (sp + delta) i.next
    | Iop (Itailcall_ind | Itailcall_imm _) -> ()
    | Iop (Icall_ind | Icall_imm _) ->
        contains_nontail_calls := true;
        analyze sp i.next
    | Iop _ ->
        analyze sp i.next
    | Ireturn -> ()
    | Iifthenelse(_, ifso, ifnot) ->
        analyze sp ifso; analyze sp ifnot; analyze sp i.next
    | Iswitch(_, branches) ->
        Array.iter (analyze sp) branches; analyze sp i.next
    | Icatch(_, handlers, body) ->
        List.iter (fun (_, handler) -> analyze sp handler) handlers;
        analyze sp body;
        analyze sp i.next
    | Iexit _ -> ()
    | Itrywith(body, handler) ->
        analyze (sp + self#trap_handler_size) body;
        analyze sp handler;
        analyze sp i.next
    | Iraise _ -> ()
 in
   analyze 0 f.fun_body;
   { contains_nontail_calls = !contains_nontail_calls;
     frame_required = self#frame_required f !contains_calls;
     extra_stack_used = !extra_space }

end
