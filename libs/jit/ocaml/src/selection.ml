# 2 "asmcomp/amd64/selection.ml"
(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 2000 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Instruction selection for the AMD64 *)

open Arch
open Proc
open Cmm
open Mach

(* Auxiliary for recognizing addressing modes *)

type addressing_expr =
    Asymbol of string
  | Alinear of expression
  | Aadd of expression * expression
  | Ascale of expression * int
  | Ascaledadd of expression * expression * int

let rec select_addr exp =
  let default = (Alinear exp, 0) in
  match exp with
    Cconst_symbol (s, _) when not !Clflags.dlcode ->
      (Asymbol s, 0)
  | Cop((Caddi | Caddv | Cadda), [arg; Cconst_int (m, _)], _)
  | Cop((Caddi | Caddv | Cadda), [Cconst_int (m, _); arg], _) ->
      let (a, n) = select_addr arg in
      if Misc.no_overflow_add n m then (a, n + m) else default
  | Cop(Csubi, [arg; Cconst_int (m, _)], _) ->
      let (a, n) = select_addr arg in
      if Misc.no_overflow_sub n m then (a, n - m) else default
  | Cop(Clsl, [arg; Cconst_int((1|2|3 as shift), _)], _) ->
      begin match select_addr arg with
      | (Alinear e, n) when Misc.no_overflow_lsl n shift ->
          (Ascale(e, 1 lsl shift), n lsl shift)
      | _ -> default
      end
  | Cop(Cmuli, [arg; Cconst_int((2|4|8 as mult), _)], _)
  | Cop(Cmuli, [Cconst_int((2|4|8 as mult), _); arg], _) ->
      begin match select_addr arg with
      | (Alinear e, n) when Misc.no_overflow_mul n mult ->
          (Ascale(e, mult), n * mult)
      | _ -> default
      end
  | Cop((Caddi | Caddv | Cadda), [arg1; arg2], _) ->
      begin match (select_addr arg1, select_addr arg2) with
          ((Alinear e1, n1), (Alinear e2, n2))
          when Misc.no_overflow_add n1 n2 ->
              (Aadd(e1, e2), n1 + n2)
        | ((Alinear e1, n1), (Ascale(e2, scale), n2))
        | ((Ascale(e2, scale), n2), (Alinear e1, n1))
          when Misc.no_overflow_add n1 n2 ->
              (Ascaledadd(e1, e2, scale), n1 + n2)
        | (_, (Ascale(e2, scale), n2)) ->
              (Ascaledadd(arg1, e2, scale), n2)
        | ((Ascale(e1, scale), n1), _) ->
              (Ascaledadd(arg2, e1, scale), n1)
        | _ ->
              (Aadd(arg1, arg2), 0)
      end
  | _ -> default

(* Special constraints on operand and result registers *)

exception Use_default

let rax = phys_reg 0
let rcx = phys_reg 5
let rdx = phys_reg 4

let pseudoregs_for_operation op arg res =
  match op with
  (* Two-address binary operations: arg.(0) and res.(0) must be the same *)
    Iintop(Iadd|Isub|Imul|Iand|Ior|Ixor) | Iaddf|Isubf|Imulf|Idivf ->
      ([|res.(0); arg.(1)|], res)
  (* One-address unary operations: arg.(0) and res.(0) must be the same *)
  | Iintop_imm((Iadd|Isub|Imul|Iand|Ior|Ixor|Ilsl|Ilsr|Iasr), _)
  | Iabsf | Inegf
  | Ispecific(Ibswap (32|64)) ->
      (res, res)
  (* For xchg, args must be a register allowing access to high 8 bit register
     (rax, rbx, rcx or rdx). Keep it simple, just force the argument in rax. *)
  | Ispecific(Ibswap 16) ->
      ([| rax |], [| rax |])
  (* For imulq, first arg must be in rax, rax is clobbered, and result is in
     rdx. *)
  | Iintop(Imulh) ->
      ([| rax; arg.(1) |], [| rdx |])
  | Ispecific(Ifloatarithmem(_,_)) ->
      let arg' = Array.copy arg in
      arg'.(0) <- res.(0);
      (arg', res)
  (* For shifts with variable shift count, second arg must be in rcx *)
  | Iintop(Ilsl|Ilsr|Iasr) ->
      ([|res.(0); rcx|], res)
  (* For div and mod, first arg must be in rax, rdx is clobbered,
     and result is in rax or rdx respectively.
     Keep it simple, just force second argument in rcx. *)
  | Iintop(Idiv) ->
      ([| rax; rcx |], [| rax |])
  | Iintop(Imod) ->
      ([| rax; rcx |], [| rdx |])
  | Icompf cond ->
      (* We need to temporarily store the result of the comparison in a
         float register, but we don't want to clobber any of the inputs
         if they would still be live after this operation -- so we
         add a fresh register as both an input and output. We don't use
         [destroyed_at_oper], because that forces us to choose a fixed
         register, which makes it more likely an extra mov would be added
         to transfer the argument to the fixed register. *)
      let treg = Reg.create Float in
      let _,is_swapped = float_cond_and_need_swap cond in
      (if is_swapped then [| arg.(0); treg |] else [| treg; arg.(1) |])
    , [| res.(0); treg |]
  (* Other instructions are regular *)
  | _ -> raise Use_default

(* If you update [inline_ops], you may need to update [is_simple_expr] and/or
   [effects_of], below. *)
let inline_ops =
  [ "sqrt"; "caml_bswap16_direct"; "caml_int32_direct_bswap";
    "caml_int64_direct_bswap"; "caml_nativeint_direct_bswap" ]

let is_immediate n = n <= 0x7FFF_FFFF && n >= -0x8000_0000

let is_immediate_natint n = n <= 0x7FFF_FFFFn && n >= -0x8000_0000n

(* The selector class *)

class selector = object (self)

inherit Selectgen.selector_generic as super

method! is_immediate op n =
  match op with
  | Iadd | Isub | Imul | Iand | Ior | Ixor | Icomp _ | Icheckbound ->
      is_immediate n
  | _ ->
      super#is_immediate op n

method is_immediate_test _cmp n = is_immediate n

method! is_simple_expr e =
  match e with
  | Cop(Cextcall (fn, _, _, _), args, _)
    when List.mem fn inline_ops ->
      (* inlined ops are simple if their arguments are *)
      List.for_all self#is_simple_expr args
  | _ ->
      super#is_simple_expr e

method! effects_of e =
  match e with
  | Cop(Cextcall(fn, _, _, _), args, _)
    when List.mem fn inline_ops ->
      Selectgen.Effect_and_coeffect.join_list_map args self#effects_of
  | _ ->
      super#effects_of e

method select_addressing _chunk exp =
  let (a, d) = select_addr exp in
  (* PR#4625: displacement must be a signed 32-bit immediate *)
  if not (is_immediate d)
  then (Iindexed 0, exp)
  else match a with
    | Asymbol s ->
        (Ibased(s, d), Ctuple [])
    | Alinear e ->
        (Iindexed d, e)
    | Aadd(e1, e2) ->
        (Iindexed2 d, Ctuple[e1; e2])
    | Ascale(e, scale) ->
        (Iscaled(scale, d), e)
    | Ascaledadd(e1, e2, scale) ->
        (Iindexed2scaled(scale, d), Ctuple[e1; e2])

method! select_store is_assign addr exp =
  match exp with
    Cconst_int (n, _dbg) when is_immediate n ->
      (Ispecific(Istore_int(Nativeint.of_int n, addr, is_assign)), Ctuple [])
  | (Cconst_natint (n, _dbg)) when is_immediate_natint n ->
      (Ispecific(Istore_int(n, addr, is_assign)), Ctuple [])
  | _ ->
      super#select_store is_assign addr exp

method! select_operation op args dbg =
  match op with
  (* Recognize the LEA instruction *)
    Caddi | Caddv | Cadda | Csubi ->
      begin match self#select_addressing Word_int (Cop(op, args, dbg)) with
        (Iindexed _, _)
      | (Iindexed2 0, _) -> super#select_operation op args dbg
      | (addr, arg) -> (Ispecific(Ilea addr), [arg])
      end
  (* Recognize float arithmetic with memory. *)
  | Caddf ->
      self#select_floatarith true Iaddf Ifloatadd args
  | Csubf ->
      self#select_floatarith false Isubf Ifloatsub args
  | Cmulf ->
      self#select_floatarith true Imulf Ifloatmul args
  | Cdivf ->
      self#select_floatarith false Idivf Ifloatdiv args
  | Cextcall("sqrt", _, _, false) ->
     begin match args with
       [Cop(Cload {memory_chunk=(Double as chunk)}, [loc], _dbg)] ->
         let (addr, arg) = self#select_addressing chunk loc in
         (Ispecific(Ifloatsqrtf addr), [arg])
     | [arg] ->
         (Ispecific Isqrtf, [arg])
     | _ ->
         assert false
     end
  (* Recognize store instructions *)
  | Cstore ((Word_int|Word_val as chunk), _init) ->
      begin match args with
        [loc; Cop(Caddi, [Cop(Cload _, [loc'], _); Cconst_int (n, _dbg)], _)]
        when loc = loc' && is_immediate n ->
          let (addr, arg) = self#select_addressing chunk loc in
          (Ispecific(Ioffset_loc(n, addr)), [arg])
      | _ ->
          super#select_operation op args dbg
      end
  | Cextcall("caml_bswap16_direct", _, _, _) ->
      (Ispecific (Ibswap 16), args)
  | Cextcall("caml_int32_direct_bswap", _, _, _) ->
      (Ispecific (Ibswap 32), args)
  | Cextcall("caml_int64_direct_bswap", _, _, _)
  | Cextcall("caml_nativeint_direct_bswap", _, _, _) ->
      (Ispecific (Ibswap 64), args)
  (* Recognize sign extension *)
  | Casr ->
      begin match args with
        [Cop(Clsl, [k; Cconst_int (32, _)], _); Cconst_int (32, _)] ->
          (Ispecific Isextend32, [k])
        | _ -> super#select_operation op args dbg
      end
  (* Recognize zero extension *)
  | Cand ->
    begin match args with
    | [arg; Cconst_int (0xffff_ffff, _)]
    | [arg; Cconst_natint (0xffff_ffffn, _)]
    | [Cconst_int (0xffff_ffff, _); arg]
    | [Cconst_natint (0xffff_ffffn, _); arg] ->
      Ispecific Izextend32, [arg]
    | _ -> super#select_operation op args dbg
    end
  | _ -> super#select_operation op args dbg

(* Recognize float arithmetic with mem *)

method select_floatarith commutative regular_op mem_op args =
  match args with
    [arg1; Cop(Cload {memory_chunk=(Double as chunk)}, [loc2], _)] ->
      let (addr, arg2) = self#select_addressing chunk loc2 in
      (Ispecific(Ifloatarithmem(mem_op, addr)),
                 [arg1; arg2])
  | [Cop(Cload {memory_chunk=(Double as chunk)}, [loc1], _); arg2]
        when commutative ->
      let (addr, arg1) = self#select_addressing chunk loc1 in
      (Ispecific(Ifloatarithmem(mem_op, addr)),
                 [arg2; arg1])
  | [arg1; arg2] ->
      (regular_op, [arg1; arg2])
  | _ ->
      assert false

(* Deal with register constraints *)

method! insert_op_debug env op dbg rs rd =
  try
    let (rsrc, rdst) = pseudoregs_for_operation op rs rd in
    self#insert_moves env rs rsrc;
    self#insert_debug env (Iop op) dbg rsrc rdst;
    self#insert_moves env rdst rd;
    rd
  with Use_default ->
    super#insert_op_debug env op dbg rs rd

end

let fundecl ~future_funcnames f =
  (new selector)#emit_fundecl ~future_funcnames f
