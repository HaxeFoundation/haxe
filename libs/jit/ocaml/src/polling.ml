(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*      Xavier Leroy and Damien Doligez, projet Cambium, INRIA Paris      *)
(*               Sadiq Jaffer, OCaml Labs Consultancy Ltd                 *)
(*          Stephen Dolan and Mark Shinwell, Jane Street Europe           *)
(*                                                                        *)
(*   Copyright 2021 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*   Copyright 2021 OCaml Labs Consultancy Ltd                            *)
(*   Copyright 2021 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open Mach
open Format_doc

module Int = Numbers.Int
module String = Misc.Stdlib.String

let function_is_assumed_to_never_poll func =
  String.starts_with ~prefix:"caml_apply" func
  || String.starts_with ~prefix:"caml_send" func

(* These are used for the poll error annotation later on*)
type polling_point = Alloc | Poll | Function_call | External_call
type error = Poll_error of (polling_point * Debuginfo.t) list

exception Error of error

(* Detection of recursive handlers that are not guaranteed to poll
   at every loop iteration. *)

(* We use a backwards dataflow analysis to compute a mapping from handlers H
   (= loop heads) to either "safe" or "unsafe".

   H is "safe" if every path starting from H goes through an Ialloc,
   Ipoll, Ireturn, Itailcall_ind or Itailcall_imm instruction.

   H is "unsafe", therefore, if starting from H we can loop infinitely
   without crossing an Ialloc or Ipoll instruction.
*)

type unsafe_or_safe = Unsafe | Safe

module Unsafe_or_safe = struct
  type t = unsafe_or_safe

  let bot = Unsafe

  let join t1 t2 =
    match t1, t2 with
    | Unsafe, Unsafe
    | Unsafe, Safe
    | Safe, Unsafe -> Unsafe
    | Safe, Safe -> Safe

  let lessequal t1 t2 =
    match t1, t2 with
    | Unsafe, Unsafe
    | Unsafe, Safe
    | Safe, Safe -> true
    | Safe, Unsafe -> false
end

module PolledLoopsAnalysis = Dataflow.Backward(Unsafe_or_safe)

let polled_loops_analysis funbody =
  let transfer i ~next ~exn =
    match i.desc with
    | Iend -> next
    | Iop (Ialloc _ | Ipoll _)
    | Iop (Itailcall_ind | Itailcall_imm _) -> Safe
    | Iop op ->
      if operation_can_raise op
      then Unsafe_or_safe.join next exn
      else next
    | Ireturn -> Safe
    | Iifthenelse _ | Iswitch _ | Icatch _ | Iexit _ | Itrywith _ -> next
    | Iraise _ -> exn
  in
  (* [exnescape] is [Safe] because we can't loop infinitely having
     returned from the function via an unhandled exception. *)
  snd (PolledLoopsAnalysis.analyze ~exnescape:Safe ~transfer funbody)

(* Detection of functions that can loop via a tail-call without going
   through a poll point. *)

(* We use a backwards dataflow analysis to compute a single value: either
   "Might_not_poll" or "Always_polls".

   "Might_not_poll" means there exists a path from the function entry to a
   Potentially Recursive Tail Call (an Itailcall_ind or
   Itailcall_imm to a forward function)
   that does not go through an Ialloc or Ipoll instruction.

   "Always_polls", therefore, means the function always polls (via Ialloc or
   Ipoll) before doing a PRTC.  This includes the case where it does not
   perform any PRTC.

   A note on Potentially Recursive Tail Calls
   ------------------------------------------

   Tail calls can create infinite loops, of course. (Consider a function
   that tail-calls itself.)  But not all tail calls need to be flagged
   as potential infinite loops.

   We optimise by making a partial ordering over Mach functions: in
   definition order within a compilation unit, and dependency
   order between compilation units. This order is acyclic, as
   OCaml does not allow circular dependencies between modules.
   It's also finite, so if there's an infinite sequence of
   function calls then something has to make a forward reference.

   Also, in such an infinite sequence of function calls, at most finitely
   many of them can be non-tail calls. (If there are infinitely many
   non-tail calls, then the program soon terminates with a stack
   overflow).

   So, every such infinite sequence must contain many forward-referencing
   tail calls.  These tail calls are the Potentially Recursive Tail Calls
   (PTRCs).  Polling only on those calls suffices.

   Several functions below take a parameter [future_funcnames]
   which is the set of functions defined "after" the current function
   in the current compilation unit.  The PTRCs are tail calls
   to known functions in [future_funcnames], or tail calls to
   unknown functions.
*)

type polls_before_prtc = Might_not_poll | Always_polls

module Polls_before_prtc = struct
  type t = polls_before_prtc

  let bot = Always_polls

  let join t1 t2 =
    match t1, t2 with
    | Might_not_poll, Might_not_poll
    | Might_not_poll, Always_polls
    | Always_polls, Might_not_poll -> Might_not_poll
    | Always_polls, Always_polls -> Always_polls

  let lessequal t1 t2 =
    match t1, t2 with
    | Always_polls, Always_polls
    | Always_polls, Might_not_poll
    | Might_not_poll, Might_not_poll -> true
    | Might_not_poll, Always_polls -> false
end

module PTRCAnalysis = Dataflow.Backward(Polls_before_prtc)

let potentially_recursive_tailcall ~future_funcnames funbody =
  let transfer i ~next ~exn =
    match i.desc with
    | Iend -> next
    | Iop (Ialloc _ | Ipoll _) -> Always_polls
    | Iop (Itailcall_ind) -> Might_not_poll  (* this is a PTRC *)
    | Iop (Itailcall_imm { func }) ->
      if String.Set.mem func future_funcnames
         || function_is_assumed_to_never_poll func
      then Might_not_poll  (* this is a PTRC *)
      else Always_polls    (* this is not a PTRC *)
    | Iop op ->
      if operation_can_raise op
      then Polls_before_prtc.join next exn
      else next
    | Ireturn -> Always_polls
    | Iifthenelse _ | Iswitch _ | Icatch _ | Iexit _ | Itrywith _ -> next
    | Iraise _ -> exn
  in
  fst (PTRCAnalysis.analyze ~transfer funbody)

(* We refer to the set of recursive handler labels that need extra polling
   as the "unguarded back edges" ("ube").

   Given the result of the analysis of recursive handlers, add [Ipoll]
   instructions at the [Iexit] instructions before unguarded back edges,
   thus ensuring that every loop contains a poll point.  Also compute whether
   the resulting function contains any [Ipoll] instructions.
*)

let add_poll i =
  Mach.instr_cons_debug (Iop (Ipoll { return_label = None })) [||] [||] i.dbg i

let instr_body handler_safe i =
  let add_unsafe_handler ube (k, _) =
    match handler_safe k with
    | Safe -> ube
    | Unsafe -> Int.Set.add k ube
  in
  let rec instr ube i =
    match i.desc with
    | Iifthenelse (test, i0, i1) ->
      { i with
        desc = Iifthenelse (test, instr ube i0, instr ube i1);
        next = instr ube i.next;
      }
    | Iswitch (index, cases) ->
      { i with
        desc = Iswitch (index, Array.map (instr ube) cases);
        next = instr ube i.next;
      }
    | Icatch (rc, hdl, body) ->
      let ube' =
        match rc with
        | Cmm.Recursive -> List.fold_left add_unsafe_handler ube hdl
        | Cmm.Nonrecursive -> ube in
      let instr_handler (k, i0) =
        let i1 = instr ube' i0 in
        (k, i1) in
      (* Since we are only interested in unguarded _back_ edges, we don't
         use [ube'] for instrumenting [body], but just [ube] instead. *)
      let body = instr ube body in
      { i with
        desc = Icatch (rc,
                       List.map instr_handler hdl,
                       body);
        next = instr ube i.next;
      }
    | Iexit k ->
      if Int.Set.mem k ube
      then add_poll i
      else i
    | Itrywith (body, hdl) ->
      { i with
        desc = Itrywith (instr ube body, instr ube hdl);
        next = instr ube i.next;
      }
    | Iend | Ireturn | Iraise _ -> i
    | Iop _ ->
      { i with next = instr ube i.next }
  in
  instr Int.Set.empty i

let find_poll_alloc_or_calls instr =
  let f_match i =
      match i.desc with
      | Iop(Ipoll _) -> Some (Poll, i.dbg)
      | Iop(Ialloc _) -> Some (Alloc, i.dbg)
      | Iop(Icall_ind | Icall_imm _ |
            Itailcall_ind | Itailcall_imm _ ) -> Some (Function_call, i.dbg)
      | Iop(Iextcall { alloc = true }) -> Some (External_call, i.dbg)
      | Iop(Imove | Ispill | Ireload | Iconst_int _ | Iconst_float _ |
            Iconst_symbol _ | Iextcall { alloc = false } | Istackoffset _ |
            Iload _ | Istore _ | Iintop _ | Iintop_imm _ | Ifloatofint |
            Iintoffloat | Inegf | Iabsf | Iaddf | Isubf | Imulf | Idivf |
            Iopaque | Ispecific _ | Idls_get | Icompf _ | Ireturn_addr) -> None
      | Iend | Ireturn | Iifthenelse _ | Iswitch _ | Icatch _ | Iexit _ |
        Itrywith _ | Iraise _ -> None
    in
  let matches = ref [] in
    Mach.instr_iter
      (fun i ->
        match f_match i with
        | Some(x) -> matches := x :: !matches
        | None -> ())
      instr;
  List.rev !matches

let instrument_fundecl ~future_funcnames:_ (f : Mach.fundecl) : Mach.fundecl =
  if function_is_assumed_to_never_poll f.fun_name then f
  else begin
    let handler_needs_poll = polled_loops_analysis f.fun_body in
    let new_body = instr_body handler_needs_poll f.fun_body in
    begin match f.fun_poll with
    | Error_poll -> begin
        match find_poll_alloc_or_calls new_body with
        | [] -> ()
        | poll_error_instrs -> raise (Error(Poll_error poll_error_instrs))
      end
    | Default_poll -> () end;
    { f with fun_body = new_body }
  end

let requires_prologue_poll ~future_funcnames ~fun_name i =
  if function_is_assumed_to_never_poll fun_name then false
  else
    match potentially_recursive_tailcall ~future_funcnames i with
    | Might_not_poll -> true
    | Always_polls -> false

(* Error report *)

let instr_type p =
  match p with
  | Poll -> "inserted poll"
  | Alloc -> "allocation"
  | Function_call -> "function call"
  | External_call -> "external call that allocates"

let report_error ppf = function
| Poll_error instrs ->
  begin
    let num_inserted_polls =
      List.fold_left
      (fun s (p,_) -> s + match p with Poll -> 1
                      | Alloc | Function_call | External_call -> 0
      ) 0 instrs in
      let num_user_polls = (List.length instrs) - num_inserted_polls in
      if num_user_polls = 0 then
        fprintf ppf "Function with poll-error attribute contains polling \
        points (inserted by the compiler)\n"
      else begin
        fprintf ppf
        "Function with poll-error attribute contains polling points:\n";
        List.iter (fun (p,dbg) ->
          begin match p with
          | Poll -> ()
          | Alloc | Function_call | External_call ->
            fprintf ppf "\t%s at " (instr_type p);
            Location.Doc.loc ppf (Debuginfo.to_location dbg);
            fprintf ppf "\n"
          end
        ) instrs;
        if num_inserted_polls > 0 then
          fprintf ppf "\t(plus compiler-inserted polling point(s) in prologue \
          and/or loop back edges)\n"
      end
  end

let () =
  Location.register_error_of_exn
    (function
      | Error err -> Some (Location.error_of_printer_file report_error err)
      | _ -> None
    )
