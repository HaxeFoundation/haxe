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

(* Transformation of Mach code into a list of pseudo-instructions. *)
open Linear

(* Cons a simple instruction (arg, res, live empty) *)

let cons_instr d n =
  { desc = d; next = n; arg = [||]; res = [||];
    dbg = Debuginfo.none; live = Reg.Set.empty }

(* Build an instruction with arg, res, dbg, live taken from
   the given Mach.instruction *)

let copy_instr d i n =
  { desc = d; next = n;
    arg = i.Mach.arg; res = i.Mach.res;
    dbg = i.Mach.dbg; live = i.Mach.live }

(*
   Label the beginning of the given instruction sequence.
   - If the sequence starts with a branch, jump over it.
   - If the sequence is the end, (tail call position), just do nothing
*)

let get_label n = match n.desc with
    Lbranch lbl -> (lbl, n)
  | Llabel lbl -> (lbl, n)
  | Lend -> (-1, n)
  | _ -> let lbl = Cmm.new_label() in (lbl, cons_instr (Llabel lbl) n)

(* Check the fallthrough label *)
let check_label n = match n.desc with
  | Lbranch lbl -> lbl
  | Llabel lbl -> lbl
  | _ -> -1


(* Add pseudo-instruction Ladjust_trap_depth in front of a continuation
   to notify assembler generation about updates to the stack as a result
   of differences in exception trap depths.
   The argument delta is the number of trap frames (not bytes). *)

let rec adjust_trap_depth delta_traps next =
  (* Simplify by merging and eliminating Ladjust_trap_depth instructions
     whenever possible. *)
  match next.desc with
  | Ladjust_trap_depth { delta_traps = k } ->
    adjust_trap_depth (delta_traps + k) next.next
  | _ ->
    if delta_traps = 0 then next
    else cons_instr (Ladjust_trap_depth { delta_traps }) next

(* Discard all instructions up to the next label.
   This function is to be called before adding a non-terminating
   instruction. *)

let rec discard_dead_code n =
  let adjust trap_depth =
    adjust_trap_depth trap_depth (discard_dead_code n.next)
  in
  match n.desc with
    Lend -> n
  | Llabel _ -> n
    (* Do not discard Lpoptrap/Lpushtrap/Ladjust_trap_depth
       or Istackoffset instructions, as this may cause a stack imbalance
       later during assembler generation. Replace them
       with pseudo-instruction Ladjust_trap_depth with the corresponding
       stack offset and eliminate dead instructions after them. *)
  | Lpoptrap -> adjust (-1)
  | Lpushtrap _ -> adjust (+1)
  | Ladjust_trap_depth { delta_traps } -> adjust delta_traps
  | Lop(Istackoffset _) ->
    (* This dead instruction cannot be replaced by Ladjust_trap_depth,
       because the units don't match: the argument of Istackoffset is in bytes,
       whereas the argument of Ladjust_trap_depth is in trap frames,
       and the size of trap frames is machine-dependant and therefore not
       available here.  *)
    { n with next = discard_dead_code n.next; }
  | _ -> discard_dead_code n.next

(*
   Add a branch in front of a continuation.
   Discard dead code in the continuation.
   Does not insert anything if we're just falling through
   or if we jump to dead code after the end of function (lbl=-1)
*)

let add_branch lbl n =
  if lbl >= 0 then
    let n1 = discard_dead_code n in
    match n1.desc with
    | Llabel lbl1 when lbl1 = lbl -> n1
    | _ -> cons_instr (Lbranch lbl) n1
  else
    discard_dead_code n


type exit_info = {
  try_depth : int;
  exit_label : (int * (int * int)) list;
  (* Association list: exit handler -> (handler label, try-nesting factor) *)
}

let find_exit_label_try_depth exit_info k =
  try
    List.assoc k exit_info.exit_label
  with
  | Not_found -> Misc.fatal_error "Linearize.find_exit_label"

let find_exit_label exit_info k =
  let (label, t) = find_exit_label_try_depth exit_info k in
  assert(t = exit_info.try_depth);
  label

let is_next_catch exit_info n =
  match exit_info.exit_label with
  | (n0,(_,t))::_  when n0=n && t = exit_info.try_depth -> true
  | _ -> false

let local_exit exit_info k =
  snd (find_exit_label_try_depth exit_info k) = exit_info.try_depth

(* Linearize an instruction [i]: add it in front of the continuation [n] *)
let linear i n contains_calls =
  let rec linear exit_info i n =
    match i.Mach.desc with
      Iend -> n
    | Iop(Itailcall_ind | Itailcall_imm _ as op) ->
        copy_instr (Lop op) i (discard_dead_code n)
    | Iop(Imove | Ireload | Ispill)
      when i.Mach.arg.(0).loc = i.Mach.res.(0).loc ->
        linear exit_info i.Mach.next n
    | Iop((Ipoll { return_label = None; _ }) as op) ->
        (* If the poll call does not already specify where to jump to after
           the poll (the expected situation in the current implementation),
           absorb any branch after the poll call into the poll call itself.
           This, in particular, optimises polls at the back edges of loops. *)
        let n = linear exit_info i.Mach.next n in
        let op, n =
          match n.desc with
          | Lbranch lbl ->
            Mach.Ipoll { return_label = Some lbl }, n.next
          | _ -> op, n
        in
        copy_instr (Lop op) i n
    | Iop op ->
        copy_instr (Lop op) i (linear exit_info i.Mach.next n)
    | Ireturn ->
        let n1 = copy_instr Lreturn i (discard_dead_code n) in
        if contains_calls
        then cons_instr Lreloadretaddr n1
        else n1
    | Iifthenelse(test, ifso, ifnot) ->
        let n1 = linear exit_info i.Mach.next n in
        begin match (ifso.Mach.desc, ifnot.Mach.desc, n1.desc) with
          Iend, _, Lbranch lbl ->
            copy_instr (Lcondbranch(test, lbl)) i (linear exit_info ifnot n1)
        | _, Iend, Lbranch lbl ->
            copy_instr
              (Lcondbranch(invert_test test, lbl))
              i
              (linear exit_info ifso n1)
        | Iexit nfail1, Iexit nfail2, _
          when is_next_catch exit_info nfail1
            && local_exit exit_info nfail2 ->
            let lbl2 = find_exit_label exit_info nfail2 in
            copy_instr
              (Lcondbranch (invert_test test, lbl2))
              i
              (linear exit_info ifso n1)
        | Iexit nfail, _, _ when local_exit exit_info nfail ->
            let n2 = linear exit_info ifnot n1
            and lbl = find_exit_label exit_info nfail in
            copy_instr (Lcondbranch(test, lbl)) i n2
        | _,  Iexit nfail, _ when local_exit exit_info nfail ->
            let n2 = linear exit_info ifso n1 in
            let lbl = find_exit_label exit_info nfail in
            copy_instr (Lcondbranch(invert_test test, lbl)) i n2
        | Iend, _, _ ->
            let (lbl_end, n2) = get_label n1 in
            copy_instr
              (Lcondbranch(test, lbl_end))
              i
              (linear exit_info ifnot n2)
        | _,  Iend, _ ->
            let (lbl_end, n2) = get_label n1 in
            copy_instr (Lcondbranch(invert_test test, lbl_end)) i
                       (linear exit_info ifso n2)
        | _, _, _ ->
          (* Should attempt branch prediction here *)
            let (lbl_end, n2) = get_label n1 in
            let (lbl_else, nelse) = get_label (linear exit_info ifnot n2) in
            copy_instr (Lcondbranch(invert_test test, lbl_else)) i
              (linear exit_info ifso (add_branch lbl_end nelse))
        end
    | Iswitch(index, cases) ->
        let lbl_cases = Array.make (Array.length cases) 0 in
        let (lbl_end, n1) = get_label(linear exit_info i.Mach.next n) in
        let n2 = ref (discard_dead_code n1) in
        for i = Array.length cases - 1 downto 0 do
          let case_linear =
            linear exit_info cases.(i) (add_branch lbl_end !n2) in
          let (lbl_case, ncase) = get_label case_linear in
          lbl_cases.(i) <- lbl_case;
          n2 := discard_dead_code ncase
        done;
        (* Switches with 1 and 2 branches have been eliminated earlier.
           Here, we do something for switches with 3 branches. *)
        if Array.length index = 3 then begin
          let fallthrough_lbl = check_label !n2 in
          let find_label n =
            let lbl = lbl_cases.(index.(n)) in
            if lbl = fallthrough_lbl then None else Some lbl in
          copy_instr (Lcondbranch3(find_label 0, find_label 1, find_label 2))
                     i !n2
        end else
          copy_instr (Lswitch(Array.map (fun n -> lbl_cases.(n)) index)) i !n2
    | Icatch(_rec_flag, handlers, body) ->
        let (lbl_end, n1) = get_label(linear exit_info i.Mach.next n) in
        (* CR mshinwell for pchambart:
           1. rename "io"
           2. Make sure the test cases cover the "Iend" cases too *)
        let labels_at_entry_to_handlers = List.map (fun (_nfail, handler) ->
            match handler.Mach.desc with
            | Iend -> lbl_end
            | _ -> Cmm.new_label ())
            handlers in
        let exit_label_add = List.map2
            (fun (nfail, _) lbl -> (nfail, (lbl, exit_info.try_depth)))
            handlers labels_at_entry_to_handlers in
        let exit_info =
          { exit_info with exit_label = exit_label_add @ exit_info.exit_label }
        in
        let n2 = List.fold_left2 (fun n (_nfail, handler) lbl_handler ->
            match handler.Mach.desc with
            | Iend -> n
            | _ ->
                cons_instr (Llabel lbl_handler)
                  (linear exit_info handler (add_branch lbl_end n)))
            n1 handlers labels_at_entry_to_handlers
        in
        let n3 = linear exit_info body (add_branch lbl_end n2) in
        n3
    | Iexit nfail ->
        let lbl, t = find_exit_label_try_depth exit_info nfail in
        assert (i.Mach.next.desc = Mach.Iend);
        let delta_traps = exit_info.try_depth - t in
        let n1 = adjust_trap_depth delta_traps n in
        let rec loop i tt =
          if t = tt then i
          else loop (cons_instr Lpoptrap i) (tt - 1)
        in
        loop (add_branch lbl n1) exit_info.try_depth
    | Itrywith(body, handler) ->
        let (lbl_join, n1) = get_label (linear exit_info i.Mach.next n) in
        let (lbl_handler, n2) =
          get_label (cons_instr Lentertrap (linear exit_info handler n1))
        in
        let exit_info =
          { exit_info with try_depth = exit_info.try_depth + 1 } in
        assert (i.Mach.arg = [| |]);
        let n3 = cons_instr (Lpushtrap { lbl_handler; })
                   (linear exit_info body
                      (cons_instr
                         Lpoptrap
                         (add_branch lbl_join n2))) in
        n3

    | Iraise k ->
        copy_instr (Lraise k) i (discard_dead_code n)
  in linear { exit_label = []; try_depth = 0 } i n

let add_prologue first_insn prologue_required =
  let tailrec_entry_point_label = Cmm.new_label () in
  let tailrec_entry_point =
    { desc = Llabel tailrec_entry_point_label;
      next = first_insn;
      arg = [| |];
      res = [| |];
      dbg = first_insn.dbg;
      live = first_insn.live;
    }
  in
  (* We expect [Lprologue] to expand to at least one instruction---as such,
     if no prologue is required, we avoid adding the instruction here.
     The reason is subtle: an empty expansion of [Lprologue] can cause
     two labels, one either side of the [Lprologue], to point at the same
     location.  This means that we lose the property (cf. [Coalesce_labels])
     that we can check if two labels point at the same location by
     comparing them for equality.  This causes trouble when the function
     whose prologue is in question lands at the top of the object file
     and we are emitting DWARF debugging information:
       foo_code_begin:
       foo:
       .L1:
       ; empty prologue
       .L2:
       ...
     If we were to emit a location list entry from L1...L2, not realising
     that they point at the same location, then the beginning and ending
     points of the range would be both equal to each other and (relative to
     "foo_code_begin") equal to zero.  This appears to confuse objdump,
     which seemingly misinterprets the entry as an end-of-list entry
     (which is encoded with two zero words), then complaining about a
     "hole in location list" (as it ignores any remaining list entries
     after the misinterpreted entry). *)
  if prologue_required then
    let prologue =
      { desc = Lprologue;
        next = tailrec_entry_point;
        arg = [| |];
        res = [| |];
        dbg = tailrec_entry_point.dbg;
        live = Reg.Set.empty;  (* will not be used *)
      }
    in
    tailrec_entry_point_label, prologue
  else
    tailrec_entry_point_label, tailrec_entry_point

let fundecl f =
  let fa = Stackframe.analyze f in
  let (fun_tailrec_entry_point_label, fun_body) =
    add_prologue (linear f.Mach.fun_body end_instr fa.frame_required)
                 fa.frame_required in
  { fun_name = f.Mach.fun_name;
    fun_args = Reg.set_of_array f.Mach.fun_args;
    fun_body;
    fun_fast = not (List.mem Cmm.Reduce_code_size f.Mach.fun_codegen_options);
    fun_dbg  = f.Mach.fun_dbg;
    fun_tailrec_entry_point_label;
    fun_contains_nontail_calls = fa.contains_nontail_calls;
    fun_num_stack_slots = f.Mach.fun_num_stack_slots;
    fun_frame_required = fa.frame_required;
    fun_extra_stack_used = fa.extra_stack_used
  }
