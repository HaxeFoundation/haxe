(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                    Mark Shinwell, Jane Street Europe                   *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open Mach
open Linear

module Make (T : Branch_relaxation_intf.S) = struct
  let label_map f =
    let map = Hashtbl.create 37 in
    let rec fill_map pc instr =
      match instr.desc with
      | Lend -> (pc, map)
      | Llabel lbl -> Hashtbl.add map lbl pc; fill_map pc instr.next
      | op -> fill_map (pc + T.instr_size f op) instr.next
    in
    fill_map 0 f.fun_body

  let branch_overflows map pc_branch lbl_dest max_branch_offset =
    let pc_dest = Hashtbl.find map lbl_dest in
    let delta = pc_dest - (pc_branch + T.offset_pc_at_branch) in
    delta <= -max_branch_offset || delta >= max_branch_offset

  let opt_branch_overflows map pc_branch opt_lbl_dest max_branch_offset =
    match opt_lbl_dest with
    | None -> false
    | Some lbl_dest ->
      branch_overflows map pc_branch lbl_dest max_branch_offset

  let instr_overflows ~code_size ~max_out_of_line_code_offset instr map pc =
    match T.Cond_branch.classify_instr instr.desc with
    | None -> false
    | Some branch ->
      let max_branch_offset =
        (* Remember to cut some slack for multi-word instructions (in the
           [Linear] sense of the word) where the branch can be anywhere in
           the middle.  12 words of slack is plenty. *)
        T.Cond_branch.max_displacement branch - 12
      in
      match instr.desc with
      | Lop (Ialloc _)
      | Lop (Ipoll { return_label = None })
      | Lop (Iintop (Icheckbound))
      | Lop (Iintop_imm (Icheckbound, _))
      | Lop (Ispecific _) ->
        (* We assume that any branches eligible for relaxation generated
           by these instructions only branch forward.  We further assume
           that any of these may branch to an out-of-line code block. *)
        code_size + max_out_of_line_code_offset - pc >= max_branch_offset
      | Lcondbranch (_, lbl) ->
        branch_overflows map pc lbl max_branch_offset
      | Lcondbranch3 (lbl0, lbl1, lbl2) ->
        opt_branch_overflows map pc lbl0 max_branch_offset
          || opt_branch_overflows map pc lbl1 max_branch_offset
          || opt_branch_overflows map pc lbl2 max_branch_offset
      | Lop (Ipoll { return_label = Some lbl }) ->
        (* A poll-and-branch instruction can branch to the label lbl,
           but also to an out-of-line code block. *)
        code_size + max_out_of_line_code_offset - pc >= max_branch_offset
        || branch_overflows map pc lbl max_branch_offset
      | _ ->
        Misc.fatal_error "Unsupported instruction for branch relaxation"

  let fixup_branches ~code_size ~max_out_of_line_code_offset map f =
    let expand_optbranch lbl n arg next =
      match lbl with
      | None -> next
      | Some l ->
        instr_cons (Lcondbranch (Iinttest_imm (Isigned Cmm.Ceq, n), l))
          arg [||] next
    in
    let rec fixup did_fix pc instr =
      match instr.desc with
      | Lend -> did_fix
      | _ ->
        let overflows =
          instr_overflows ~code_size ~max_out_of_line_code_offset instr map pc
        in
        if not overflows then
          fixup did_fix (pc + T.instr_size f instr.desc) instr.next
        else
          match instr.desc with
          | Lop (Ipoll { return_label }) ->
            instr.desc <- T.relax_poll ~return_label;
            fixup true (pc + T.instr_size f instr.desc) instr.next
          | Lop (Ialloc { bytes = num_bytes; dbginfo }) ->
            instr.desc <- T.relax_allocation ~num_bytes ~dbginfo;
            fixup true (pc + T.instr_size f instr.desc) instr.next
          | Lop (Iintop (Icheckbound)) ->
            instr.desc <- T.relax_intop_checkbound ();
            fixup true (pc + T.instr_size f instr.desc) instr.next
          | Lop (Iintop_imm (Icheckbound, bound)) ->
            instr.desc
              <- T.relax_intop_imm_checkbound ~bound;
            fixup true (pc + T.instr_size f instr.desc) instr.next
          | Lop (Ispecific specific) ->
            instr.desc <- T.relax_specific_op specific;
            fixup true (pc + T.instr_size f instr.desc) instr.next
          | Lcondbranch (test, lbl) ->
            let lbl2 = Cmm.new_label() in
            let cont =
              instr_cons (Lbranch lbl) [||] [||]
                (instr_cons (Llabel lbl2) [||] [||] instr.next)
            in
            instr.desc <- Lcondbranch (invert_test test, lbl2);
            instr.next <- cont;
            fixup true (pc + T.instr_size f instr.desc) instr.next
          | Lcondbranch3 (lbl0, lbl1, lbl2) ->
            let cont =
              expand_optbranch lbl0 0 instr.arg
                (expand_optbranch lbl1 1 instr.arg
                  (expand_optbranch lbl2 2 instr.arg instr.next))
            in
            instr.desc <- cont.desc;
            instr.next <- cont.next;
            fixup true pc instr
          | _ ->
            (* Any other instruction has already been rejected in
               [instr_overflows] above.
               We can *never* get here. *)
            assert false
    in
    fixup false 0 f.fun_body

  (* Iterate branch expansion till all conditional branches are OK *)

  let rec relax f ~max_out_of_line_code_offset =
    let min_of_max_branch_offsets =
      List.fold_left (fun min_of_max_branch_offsets branch ->
          Int.min min_of_max_branch_offsets
            (T.Cond_branch.max_displacement branch))
        max_int T.Cond_branch.all
    in
    let (code_size, map) = label_map f in
    if code_size >= min_of_max_branch_offsets
        && fixup_branches ~code_size ~max_out_of_line_code_offset map f
    then relax f ~max_out_of_line_code_offset
    else ()
end
