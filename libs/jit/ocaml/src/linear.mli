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

type label = Cmm.label

type instruction =
  { mutable desc: instruction_desc;
    mutable next: instruction;
    arg: Reg.t array;
    res: Reg.t array;
    dbg: Debuginfo.t;
    live: Reg.Set.t }

and instruction_desc =
  | Lprologue
  | Lend
  | Lop of Mach.operation
  | Lreloadretaddr
  | Lreturn
  | Llabel of label
  | Lbranch of label
  | Lcondbranch of Mach.test * label
  | Lcondbranch3 of label option * label option * label option
  | Lswitch of label array
  | Lentertrap
  | Ladjust_trap_depth of { delta_traps : int; }
  | Lpushtrap of { lbl_handler : label; }
  | Lpoptrap
  | Lraise of Lambda.raise_kind

val has_fallthrough :  instruction_desc -> bool
val end_instr: instruction
val instr_cons:
  instruction_desc -> Reg.t array -> Reg.t array -> instruction -> instruction
val invert_test: Mach.test -> Mach.test

type fundecl =
  { fun_name: string;
    fun_args: Reg.Set.t;
    fun_body: instruction;
    fun_fast: bool;
    fun_dbg : Debuginfo.t;
    fun_tailrec_entry_point_label : label;
    fun_contains_nontail_calls: bool;
    fun_num_stack_slots: int array;
    fun_frame_required: bool;
    fun_extra_stack_used: int
  }
