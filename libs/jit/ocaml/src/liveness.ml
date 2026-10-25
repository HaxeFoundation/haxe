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

(* Liveness analysis.
   Annotate mach code with the set of regs live at each point. *)

open Mach

module Domain = struct
  type t = Reg.Set.t
  let bot = Reg.Set.empty
  let join = Reg.Set.union
  let lessequal = Reg.Set.subset
end

module Analyzer = Dataflow.Backward(Domain)

let transfer i ~next ~exn =
  match i.desc with
  | Ireturn | Iop(Itailcall_ind) | Iop(Itailcall_imm _) ->
      i.live <- Reg.Set.empty; (* no regs are live across *)
      Reg.set_of_array i.arg
  | Iop op ->
      if operation_is_pure op                 (* no side effects *)
      && Reg.disjoint_set_array next i.res    (* results are not used after *)
      then begin
        (* This operation is dead code.  Ignore its arguments. *)
        i.live <- next;
        next
      end else begin
        let across1 = Reg.diff_set_array next i.res in
        let across =
          (* Operations that can raise an exception (function calls,
             bounds checks, allocations) can branch to the
             nearest enclosing try ... with.
             Hence, everything that must be live at the beginning of
             the exception handler must also be live across this instr. *)
          if operation_can_raise op
          then Reg.Set.union across1 exn
          else across1 in
        i.live <- across;
        Reg.add_set_array across i.arg
      end
  | Iifthenelse _
  | Iswitch _ ->
      i.live <- next;
      Reg.add_set_array next i.arg
  | Iend | Icatch _ | Iexit _ | Itrywith _ ->
      i.live <- next;
      next
  | Iraise _ ->
      i.live <- exn;
      Reg.add_set_array exn i.arg

let exnhandler before_handler =
  Reg.Set.remove Proc.loc_exn_bucket before_handler

let fundecl f =
  let (initially_live, _) =
    Analyzer.analyze ~exnhandler ~transfer f.fun_body in
  (* Sanity check: only function parameters can be live at entrypoint *)
  let wrong_live = Reg.Set.diff initially_live (Reg.set_of_array f.fun_args) in
  if not (Reg.Set.is_empty wrong_live) then begin
    Misc.fatal_errorf "@[Liveness.fundecl:@\n%a@]"
      Printmach.regset wrong_live
  end
