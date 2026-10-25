(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Gallium, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 2014 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Dead code elimination: remove pure instructions whose results are
   not used. *)

open Mach

module Int = Numbers.Int

type d = {
  i : instruction;   (* optimized instruction *)
  regs : Reg.Set.t;  (* a set of registers live "before" instruction [i] *)
  exits : Int.Set.t;  (* indexes of Iexit instructions "live before" [i] *)
}

let append a b =
  let rec append a b =
    match a.desc with
    | Iend -> b
    | _ -> { a with next = append a.next b }
  in
  match b.desc with
  | Iend -> a
  | _ -> append a b

let rec deadcode i =
  match i.desc with
  | Iend | Ireturn | Iop(Itailcall_ind) | Iop(Itailcall_imm _) | Iraise _ ->
      let regs = Reg.add_set_array i.live i.arg in
      { i; regs; exits = Int.Set.empty; }
  | Iop op ->
      let s = deadcode i.next in
      if operation_is_pure op                  (* no side effects *)
      && Reg.disjoint_set_array s.regs i.res   (* results are not used after *)
      then begin
        assert (Array.length i.res > 0);  (* sanity check *)
        s
      end else begin
        { i = {i with next = s.i};
          regs = Reg.add_set_array i.live i.arg;
          exits = s.exits;
        }
      end
  | Iifthenelse(test, ifso, ifnot) ->
      let ifso' = deadcode ifso in
      let ifnot' = deadcode ifnot in
      let s = deadcode i.next in
      { i = {i with desc = Iifthenelse(test, ifso'.i, ifnot'.i); next = s.i};
        regs = Reg.add_set_array i.live i.arg;
        exits = Int.Set.union s.exits
                  (Int.Set.union ifso'.exits ifnot'.exits);
      }
  | Iswitch(index, cases) ->
      let dc = Array.map deadcode cases in
      let cases' = Array.map (fun c -> c.i) dc in
      let s = deadcode i.next in
      { i = {i with desc = Iswitch(index, cases'); next = s.i};
        regs = Reg.add_set_array i.live i.arg;
        exits = Array.fold_left
                  (fun acc c -> Int.Set.union acc c.exits) s.exits dc;
      }
  | Icatch(rec_flag, handlers, body) ->
    let body' = deadcode body in
    let s = deadcode i.next in
    let handlers' = Int.Map.map deadcode (Int.Map.of_list handlers) in
    (* Previous passes guarantee that indexes of handlers are unique
       across the entire function and Iexit instructions refer
       to the correctly scoped handlers.
       We do not rely on it here, for safety. *)
    let rec add_live nfail (live_exits, used_handlers) =
      if Int.Set.mem nfail live_exits then
        (live_exits, used_handlers)
      else
        let live_exits = Int.Set.add nfail live_exits in
        match Int.Map.find_opt nfail handlers' with
        | None -> (live_exits, used_handlers)
        | Some handler ->
          let used_handlers = (nfail, handler) :: used_handlers in
          match rec_flag with
          | Cmm.Nonrecursive -> (live_exits, used_handlers)
          | Cmm.Recursive ->
            Int.Set.fold add_live handler.exits (live_exits, used_handlers)
    in
    let live_exits, used_handlers =
      Int.Set.fold add_live body'.exits (Int.Set.empty, [])
    in
    (* Remove exits that are going out of scope. *)
    let used_handler_indexes = Int.Set.of_list (List.map fst used_handlers) in
    let live_exits = Int.Set.diff live_exits used_handler_indexes in
    (* For non-recursive catch, live exits referenced in handlers are free. *)
    let live_exits =
      match rec_flag with
      | Cmm.Recursive -> live_exits
      | Cmm.Nonrecursive ->
        List.fold_left (fun exits (_,h) -> Int.Set.union h.exits exits)
          live_exits
          used_handlers
    in
    let exits = Int.Set.union s.exits live_exits in
    begin match used_handlers with
    | [] -> (* Simplify catch without handlers *)
      { i = append body'.i s.i;
        regs = body'.regs;
        exits;
      }
    | _ ->
      let handlers = List.map (fun (n,h) -> (n,h.i)) used_handlers in
      { i = { i with desc = Icatch(rec_flag, handlers, body'.i); next = s.i };
        regs = i.live;
        exits;
      }
    end
  | Iexit nfail ->
      { i;  regs = i.live; exits = Int.Set.singleton nfail; }
  | Itrywith(body, handler) ->
      let body' = deadcode body in
      let handler' = deadcode handler in
      let s = deadcode i.next in
      { i = {i with desc = Itrywith(body'.i, handler'.i); next = s.i};
        regs = i.live;
        exits = Int.Set.union s.exits
                  (Int.Set.union body'.exits handler'.exits);
      }

let fundecl f =
  let new_body = deadcode f.fun_body in
  {f with fun_body = new_body.i}
