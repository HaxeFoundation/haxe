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

(* Pretty-printing of linearized machine code *)

open Format
open Mach
open Printmach
open Linear

let label ppf l =
  Format.fprintf ppf "L%i" l

let instr ppf i =
  begin match i.desc with
  | Lend -> ()
  | Lprologue ->
      fprintf ppf "prologue"
  | Lop op ->
      begin match op with
      | Ialloc _ | Ipoll _ | Icall_ind | Icall_imm _ | Iextcall _ ->
          fprintf ppf "@[<1>{%a}@]@," regsetaddr i.live
      | _ -> ()
      end;
      operation op i.arg ppf i.res
  | Lreloadretaddr ->
      fprintf ppf "reload retaddr"
  | Lreturn ->
      fprintf ppf "return %a" regs i.arg
  | Llabel lbl ->
      fprintf ppf "%a:" label lbl
  | Lbranch lbl ->
      fprintf ppf "goto %a" label lbl
  | Lcondbranch(tst, lbl) ->
      fprintf ppf "if %a goto %a" (test tst) i.arg label lbl
  | Lcondbranch3(lbl0, lbl1, lbl2) ->
      fprintf ppf "switch3 %a" reg i.arg.(0);
      let case n = function
      | None -> ()
      | Some lbl ->
         fprintf ppf "@,case %i: goto %a" n label lbl in
      case 0 lbl0; case 1 lbl1; case 2 lbl2;
      fprintf ppf "@,endswitch"
  | Lswitch lblv ->
      fprintf ppf "switch %a" reg i.arg.(0);
      for i = 0 to Array.length lblv - 1 do
       fprintf ppf "case %i: goto %a" i label lblv.(i)
      done;
      fprintf ppf "@,endswitch"
  | Lentertrap ->
      fprintf ppf "enter trap"
  | Ladjust_trap_depth { delta_traps } ->
      fprintf ppf "adjust trap depth by %d traps" delta_traps
  | Lpushtrap { lbl_handler; } ->
      fprintf ppf "push trap %a" label lbl_handler
  | Lpoptrap ->
      fprintf ppf "pop trap"
  | Lraise k ->
      fprintf ppf "%s %a" (Lambda.raise_kind k) reg i.arg.(0)
  end;
  if not (Debuginfo.is_none i.dbg) && !Clflags.locations then
    fprintf ppf " %s" (Debuginfo.to_string i.dbg)

let rec all_instr ppf i =
  match i.desc with
  | Lend -> ()
  | _ -> fprintf ppf "%a@,%a" instr i all_instr i.next

let fundecl ppf f =
  let dbg =
    if Debuginfo.is_none f.fun_dbg || not !Clflags.locations then
      ""
    else
      " " ^ Debuginfo.to_string f.fun_dbg in
  fprintf ppf "@[<v 2>%s:%s@,%a@]" f.fun_name dbg all_instr f.fun_body
