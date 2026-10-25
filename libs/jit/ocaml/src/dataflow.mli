(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cambium, INRIA Paris                  *)
(*                                                                        *)
(*   Copyright 2021 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* An abstract domain for dataflow analysis.  Defines a type [t]
   of abstractions, with lattice operations. *)

module type DOMAIN = sig
  type t
  val bot: t
  val join: t -> t -> t
  val lessequal: t -> t -> bool
end

(* Build a backward dataflow analysis engine for the given domain. *)

module Backward(D: DOMAIN) : sig

  val analyze: ?exnhandler: (D.t -> D.t) ->
               ?exnescape: D.t ->
               transfer: (Mach.instruction -> next: D.t -> exn: D.t -> D.t) ->
               Mach.instruction ->
               D.t * (int -> D.t)

  (* [analyze ~exnhandler ~transfer instr] performs a backward dataflow
     analysis on the Mach instruction [instr], typically a function body.

     It returns a pair of
     - the abstract state at the function entry point;
     - a mapping from catch handler label to the abstract state at the
       beginning of the handler with this label.

     The [transfer] function is called as [transfer i ~next ~exn].
     - [i] is a sub-instruction of [instr].
     - [next] is the abstract state "after" the instruction for
       normal control flow, falling through the successor(s) of [i].
     - [exn] is the abstract state "after" the instruction for
       exceptional control flow, branching to the nearest exception handler
       or exiting the function with an unhandled exception.

     The [transfer] function, then, returns the abstract state "before"
     the instruction.  The dataflow analysis will, then, propagate this
     state "before" as the state "after" the predecessor instructions.

     For compound instructions like [Iifthenelse], the [next] abstract
     value that is passed to [transfer] is not the abstract state at
     the end of the compound instruction (e.g. after the "then" and "else"
     branches have joined), but the join of the abstract states at
     the beginning of the sub-instructions.  More precisely:
     - for [Iifthenelse(tst, ifso, ifnot)], it's the join of the
       abstract states at the beginning of the [ifso] and [ifnot]
       branches;
     - for [Iswitch(tbl, cases)], it's the join of the abstract states
       at the beginning of the [cases] branches;
     - for [Icatch(recflag, body, handlers)] and [Itrywith(body, handler)],
       it's the abstract state at the beginning of [body].

     The [transfer] function is called for every sub-instruction of [instr],
     as many times as needed to reach a fixpoint.  Hence, it can record
     the results of the analysis at each sub-instruction in a mutable
     data structure.  For instance, the transfer function for liveness
     analysis updates the [live] fields of instructions as a side
     effect.

     The optional [exnhandler] argument deals with exception handlers.
     This is a function that transforms the abstract state at the
     beginning of an exception handler into the exceptional abstract
     state for the instructions within the body of the handler.
     Typically, for liveness analysis, it takes the registers live at
     the beginning of the handler and removes the register
     [Proc.loc_exn_bucket] that carries the exception value.  If not
     specified, [exnhandler] defaults to the identity function.

     The optional [exnescape] argument deals with unhandled exceptions.
     It is the abstract state corresponding to exiting the function on an
     unhandled exception.  It defaults to [D.bot].
  *)

end
