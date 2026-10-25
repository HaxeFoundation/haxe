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

[@@@ocaml.warning "+a-4-9-40-41-42-44-45"]

module V = Backend_var
module VP = Backend_var.With_provenance
open Cmm
open Arch

(* Local binding of complex expressions *)

let bind name arg fn =
  match arg with
    Cvar _ | Cconst_int _ | Cconst_natint _ | Cconst_symbol _ -> fn arg
  | _ -> let id = V.create_local name in Clet(VP.create id, arg, fn (Cvar id))

let bind_load name arg fn =
  match arg with
  | Cop(Cload _, [Cvar _], _) -> fn arg
  | _ -> bind name arg fn

let bind_nonvar name arg fn =
  match arg with
    Cconst_int _ | Cconst_natint _ | Cconst_symbol _ -> fn arg
  | _ -> let id = V.create_local name in Clet(VP.create id, arg, fn (Cvar id))

let caml_black = Nativeint.shift_left (Nativeint.of_int 3) 8
    (* cf. runtime/caml/gc.h *)

(* Loads *)

let mk_load_immut memory_chunk =
  Cload {memory_chunk; mutability=Immutable; is_atomic=false}

let mk_load_mut memory_chunk =
  Cload {memory_chunk; mutability=Mutable; is_atomic=false}

let mk_load_atomic memory_chunk =
  Cload {memory_chunk; mutability=Mutable; is_atomic=true}

(* Block headers. Meaning of the tag field: see stdlib/obj.ml *)

let floatarray_tag dbg = Cconst_int (Obj.double_array_tag, dbg)

let block_header tag sz =
  Nativeint.add (Nativeint.shift_left (Nativeint.of_int sz) 10)
                (Nativeint.of_int tag)
(* Static data corresponding to "value"s must be marked black in case we are
   in no-naked-pointers mode.  See [caml_darken] and the code below that emits
   structured constants and static module definitions. *)
let black_block_header tag sz = Nativeint.logor (block_header tag sz) caml_black
let white_closure_header sz = block_header Obj.closure_tag sz
let black_closure_header sz = black_block_header Obj.closure_tag sz
let infix_header ofs = block_header Obj.infix_tag ofs
let float_header = block_header Obj.double_tag (size_float / size_addr)
let floatarray_header len =
  (* Zero-sized float arrays have tag zero for consistency with
     [caml_alloc_float_array]. *)
  assert (len >= 0);
  if len = 0 then block_header 0 0
  else block_header Obj.double_array_tag (len * size_float / size_addr)
let string_header len =
      block_header Obj.string_tag ((len + size_addr) / size_addr)
let boxedint32_header = block_header Obj.custom_tag 2
let boxedint64_header = block_header Obj.custom_tag (1 + 8 / size_addr)
let boxedintnat_header = block_header Obj.custom_tag 2
let caml_nativeint_ops = "caml_nativeint_ops"
let caml_int32_ops = "caml_int32_ops"
let caml_int64_ops = "caml_int64_ops"

let pos_arity_in_closinfo = 8 * size_addr - 8
       (* arity = the top 8 bits of the closinfo word *)

let closure_info ~arity ~startenv =
  assert (-128 <= arity && arity <= 127);
  assert (0 <= startenv && startenv < 1 lsl (pos_arity_in_closinfo - 1));
  Nativeint.(add (shift_left (of_int arity) pos_arity_in_closinfo)
                 (add (shift_left (of_int startenv) 1)
                      1n))

let alloc_float_header dbg = Cconst_natint (float_header, dbg)
let alloc_floatarray_header len dbg = Cconst_natint (floatarray_header len, dbg)
let alloc_closure_header sz dbg = Cconst_natint (white_closure_header sz, dbg)
let alloc_infix_header ofs dbg = Cconst_natint (infix_header ofs, dbg)
let alloc_closure_info ~arity ~startenv dbg =
  Cconst_natint (closure_info ~arity ~startenv, dbg)
let alloc_boxedint32_header dbg = Cconst_natint (boxedint32_header, dbg)
let alloc_boxedint64_header dbg = Cconst_natint (boxedint64_header, dbg)
let alloc_boxedintnat_header dbg = Cconst_natint (boxedintnat_header, dbg)

(* Integers *)

let max_repr_int = max_int asr 1
let min_repr_int = min_int asr 1

let int_const dbg n =
  if n <= max_repr_int && n >= min_repr_int
  then Cconst_int((n lsl 1) + 1, dbg)
  else Cconst_natint
          (Nativeint.add (Nativeint.shift_left (Nativeint.of_int n) 1) 1n, dbg)

let natint_const_untagged dbg n =
  if n > Nativeint.of_int max_int
  || n < Nativeint.of_int min_int
  then Cconst_natint (n,dbg)
  else Cconst_int (Nativeint.to_int n, dbg)

let cint_const n =
  Cint(Nativeint.add (Nativeint.shift_left (Nativeint.of_int n) 1) 1n)

let targetint_const n =
  Targetint.add (Targetint.shift_left (Targetint.of_int n) 1)
    Targetint.one

let add_no_overflow n x c dbg =
  let d = n + x in
  if d = 0 then c else Cop(Caddi, [c; Cconst_int (d, dbg)], dbg)

let rec add_const c n dbg =
  if n = 0 then c
  else match c with
  | Cconst_int (x, _) when Misc.no_overflow_add x n -> Cconst_int (x + n, dbg)
  | Cop(Caddi, [Cconst_int (x, _); c], _)
    when Misc.no_overflow_add n x ->
      add_no_overflow n x c dbg
  | Cop(Caddi, [c; Cconst_int (x, _)], _)
    when Misc.no_overflow_add n x ->
      add_no_overflow n x c dbg
  | Cop(Csubi, [Cconst_int (x, _); c], _) when Misc.no_overflow_add n x ->
      Cop(Csubi, [Cconst_int (n + x, dbg); c], dbg)
  | Cop(Csubi, [c; Cconst_int (x, _)], _) when Misc.no_overflow_sub n x ->
      add_const c (n - x) dbg
  | c -> Cop(Caddi, [c; Cconst_int (n, dbg)], dbg)

let incr_int c dbg = add_const c 1 dbg
let decr_int c dbg = add_const c (-1) dbg

let rec add_int c1 c2 dbg =
  match (c1, c2) with
  | (Cconst_int (n, _), c) | (c, Cconst_int (n, _)) ->
      add_const c n dbg
  | (Cop(Caddi, [c1; Cconst_int (n1, _)], _), c2) ->
      add_const (add_int c1 c2 dbg) n1 dbg
  | (c1, Cop(Caddi, [c2; Cconst_int (n2, _)], _)) ->
      add_const (add_int c1 c2 dbg) n2 dbg
  | (_, _) ->
      Cop(Caddi, [c1; c2], dbg)

let rec sub_int c1 c2 dbg =
  match (c1, c2) with
  | (c1, Cconst_int (n2, _)) when n2 <> min_int ->
      add_const c1 (-n2) dbg
  | (c1, Cop(Caddi, [c2; Cconst_int (n2, _)], _)) when n2 <> min_int ->
      add_const (sub_int c1 c2 dbg) (-n2) dbg
  | (Cop(Caddi, [c1; Cconst_int (n1, _)], _), c2) ->
      add_const (sub_int c1 c2 dbg) n1 dbg
  | (c1, c2) ->
      Cop(Csubi, [c1; c2], dbg)

let rec lsl_int c1 c2 dbg =
  match (c1, c2) with
  | (Cop(Clsl, [c; Cconst_int (n1, _)], _), Cconst_int (n2, _))
    when n1 > 0 && n2 > 0 && n1 + n2 < size_int * 8 ->
      Cop(Clsl, [c; Cconst_int (n1 + n2, dbg)], dbg)
  | (Cop(Caddi, [c1; Cconst_int (n1, _)], _), Cconst_int (n2, _))
    when Misc.no_overflow_lsl n1 n2 ->
      add_const (lsl_int c1 c2 dbg) (n1 lsl n2) dbg
  | (_, _) ->
      Cop(Clsl, [c1; c2], dbg)

let is_power2 n = n = 1 lsl Misc.log2 n

and mult_power2 c n dbg = lsl_int c (Cconst_int (Misc.log2 n, dbg)) dbg

let rec mul_int c1 c2 dbg =
  match (c1, c2) with
  | (c, Cconst_int (0, _)) | (Cconst_int (0, _), c) ->
      Csequence (c, Cconst_int (0, dbg))
  | (c, Cconst_int (1, _)) | (Cconst_int (1, _), c) ->
      c
  | (c, Cconst_int(-1, _)) | (Cconst_int(-1, _), c) ->
      sub_int (Cconst_int (0, dbg)) c dbg
  | (c, Cconst_int (n, _)) when is_power2 n -> mult_power2 c n dbg
  | (Cconst_int (n, _), c) when is_power2 n -> mult_power2 c n dbg
  | (Cop(Caddi, [c; Cconst_int (n, _)], _), Cconst_int (k, _)) |
    (Cconst_int (k, _), Cop(Caddi, [c; Cconst_int (n, _)], _))
    when Misc.no_overflow_mul n k ->
      add_const (mul_int c (Cconst_int (k, dbg)) dbg) (n * k) dbg
  | (c1, c2) ->
      Cop(Cmuli, [c1; c2], dbg)


let ignore_low_bit_int = function
    Cop(Caddi,
        [(Cop(Clsl, [_; Cconst_int (n, _)], _) as c); Cconst_int (1, _)], _)
      when n > 0
      -> c
  | Cop(Cor, [c; Cconst_int (1, _)], _) -> c
  | c -> c

(* removes the 1-bit sign-extension left by untag_int (tag_int c) *)
let ignore_high_bit_int = function
    Cop(Casr,
        [Cop(Clsl, [c; Cconst_int (1, _)], _); Cconst_int (1, _)], _) -> c
  | c -> c

let lsr_int c1 c2 dbg =
  match c2 with
    Cconst_int (0, _) ->
      c1
  | Cconst_int (n, _) when n > 0 ->
      Cop(Clsr, [ignore_low_bit_int c1; c2], dbg)
  | _ ->
      Cop(Clsr, [c1; c2], dbg)

let asr_int c1 c2 dbg =
  match c2 with
    Cconst_int (0, _) ->
      c1
  | Cconst_int (n, _) when n > 0 ->
      Cop(Casr, [ignore_low_bit_int c1; c2], dbg)
  | _ ->
      Cop(Casr, [c1; c2], dbg)

let tag_int i dbg =
  match i with
    Cconst_int (n, _) ->
      int_const dbg n
  | Cop(Casr, [c; Cconst_int (n, _)], _) when n > 0 ->
      Cop(Cor,
        [asr_int c (Cconst_int (n - 1, dbg)) dbg; Cconst_int (1, dbg)],
        dbg)
  | c ->
      incr_int (lsl_int c (Cconst_int (1, dbg)) dbg) dbg

let untag_int i dbg =
  match i with
    Cconst_int (n, _) -> Cconst_int(n asr 1, dbg)
  | Cop(Cor, [Cop(Casr, [c; Cconst_int (n, _)], _); Cconst_int (1, _)], _)
    when n > 0 && n < size_int * 8 ->
      Cop(Casr, [c; Cconst_int (n+1, dbg)], dbg)
  | Cop(Cor, [Cop(Clsr, [c; Cconst_int (n, _)], _); Cconst_int (1, _)], _)
    when n > 0 && n < size_int * 8 ->
      Cop(Clsr, [c; Cconst_int (n+1, dbg)], dbg)
  | c -> asr_int c (Cconst_int (1, dbg)) dbg

let mk_if_then_else dbg cond ifso_dbg ifso ifnot_dbg ifnot =
  match cond with
  | Cconst_int (0, _) -> ifnot
  | Cconst_int (1, _) -> ifso
  | _ ->
    Cifthenelse(cond, ifso_dbg, ifso, ifnot_dbg, ifnot, dbg)

let mk_not dbg cmm =
  match cmm with
  | Cop(Caddi,
        [Cop(Clsl, [c; Cconst_int (1, _)], _); Cconst_int (1, _)], dbg') ->
    begin
      match c with
      | Cop(Ccmpi cmp, [c1; c2], dbg'') ->
          tag_int
            (Cop(Ccmpi (negate_integer_comparison cmp), [c1; c2], dbg'')) dbg'
      | Cop(Ccmpa cmp, [c1; c2], dbg'') ->
          tag_int
            (Cop(Ccmpa (negate_integer_comparison cmp), [c1; c2], dbg'')) dbg'
      | Cop(Ccmpf cmp, [c1; c2], dbg'') ->
          tag_int
            (Cop(Ccmpf (negate_float_comparison cmp), [c1; c2], dbg'')) dbg'
      | _ ->
        (* 0 -> 3, 1 -> 1 *)
        Cop(Csubi,
            [Cconst_int (3, dbg); Cop(Clsl, [c; Cconst_int (1, dbg)], dbg)],
            dbg)
    end
  | Cconst_int (3, _) -> Cconst_int (1, dbg)
  | Cconst_int (1, _) -> Cconst_int (3, dbg)
  | c ->
      (* 1 -> 3, 3 -> 1 *)
      Cop(Csubi, [Cconst_int (4, dbg); c], dbg)

let mk_compare_ints dbg a1 a2 =
  match (a1,a2) with
  | Cconst_int (c1, _), Cconst_int (c2, _) ->
     int_const dbg (Int.compare c1 c2)
  | Cconst_natint (c1, _), Cconst_natint (c2, _) ->
     int_const dbg (Nativeint.compare c1 c2)
  | Cconst_int (c1, _), Cconst_natint (c2, _) ->
     int_const dbg Nativeint.(compare (of_int c1) c2)
  | Cconst_natint (c1, _), Cconst_int (c2, _) ->
     int_const dbg Nativeint.(compare c1 (of_int c2))
  | a1, a2 -> begin
      bind "int_cmp" a2 (fun a2 ->
        bind "int_cmp" a1 (fun a1 ->
          let op1 = Cop(Ccmpi(Cgt), [a1; a2], dbg) in
          let op2 = Cop(Ccmpi(Clt), [a1; a2], dbg) in
          tag_int(sub_int op1 op2 dbg) dbg))
    end

let mk_compare_floats dbg a1 a2 =
  bind "float_cmp" a2 (fun a2 ->
    bind "float_cmp" a1 (fun a1 ->
      let op1 = Cop(Ccmpf(CFgt), [a1; a2], dbg) in
      let op2 = Cop(Ccmpf(CFlt), [a1; a2], dbg) in
      let op3 = Cop(Ccmpf(CFeq), [a1; a1], dbg) in
      let op4 = Cop(Ccmpf(CFeq), [a2; a2], dbg) in
      (* If both operands a1 and a2 are not NaN, then op3 = op4 = 1,
         and the result is op1 - op2.
         If at least one of the operands is NaN,
         then op1 = op2 = 0, and the result is op3 - op4,
         which orders NaN before other values.
         To detect if the operand is NaN, we use the property:
         for all x, NaN is not equal to x, even if x is NaN.
         Therefore, op3 is 0 if and only if a1 is NaN,
         and op4 is 0 if and only if a2 is NaN.
         See also caml_float_compare_unboxed in runtime/floats.c  *)
      tag_int (add_int (sub_int op1 op2 dbg) (sub_int op3 op4 dbg) dbg) dbg))

let create_loop body dbg =
  let cont = Lambda.next_raise_count () in
  let call_cont = Cexit (cont, []) in
  let body = Csequence (body, call_cont) in
  Ccatch (Recursive, [cont, [], body, dbg], call_cont)

(* Turning integer divisions into multiply-high then shift.
   The [division_parameters] function is used in module Emit for
   those target platforms that support this optimization. *)

(* Unsigned comparison between native integers. *)

let ucompare x y = Nativeint.(compare (add x min_int) (add y min_int))

(* Unsigned division and modulus at type nativeint.
   Algorithm: Hacker's Delight section 9.3 *)

let udivmod n d = Nativeint.(
  if d < 0n then
    if ucompare n d < 0 then (0n, n) else (1n, sub n d)
  else begin
    let q = shift_left (div (shift_right_logical n 1) d) 1 in
    let r = sub n (mul q d) in
    if ucompare r d >= 0 then (succ q, sub r d) else (q, r)
  end)

(* Compute division parameters.
   Algorithm: Hacker's Delight chapter 10, fig 10-1. *)

let divimm_parameters d = Nativeint.(
  assert (d > 0n);
  let twopsm1 = min_int in (* 2^31 for 32-bit archs, 2^63 for 64-bit archs *)
  let nc = sub (pred twopsm1) (snd (udivmod twopsm1 d)) in
  let rec loop p (q1, r1) (q2, r2) =
    let p = p + 1 in
    let q1 = shift_left q1 1 and r1 = shift_left r1 1 in
    let (q1, r1) =
      if ucompare r1 nc >= 0 then (succ q1, sub r1 nc) else (q1, r1) in
    let q2 = shift_left q2 1 and r2 = shift_left r2 1 in
    let (q2, r2) =
      if ucompare r2 d >= 0 then (succ q2, sub r2 d) else (q2, r2) in
    let delta = sub d r2 in
    if ucompare q1 delta < 0 || (q1 = delta && r1 = 0n)
    then loop p (q1, r1) (q2, r2)
    else (succ q2, p - size)
  in loop (size - 1) (udivmod twopsm1 nc) (udivmod twopsm1 d))

(* The result [(m, p)] of [divimm_parameters d] satisfies the following
   inequality:

      2^(wordsize + p) < m * d <= 2^(wordsize + p) + 2^(p + 1)    (i)

   from which it follows that

      floor(n / d) = floor(n * m / 2^(wordsize+p))
                              if 0 <= n < 2^(wordsize-1)
      ceil(n / d) = floor(n * m / 2^(wordsize+p)) + 1
                              if -2^(wordsize-1) <= n < 0

   The correctness condition (i) above can be checked by the code below.
   It was exhaustively tested for values of d from 2 to 10^9 in the
   wordsize = 64 case.

let add2 (xh, xl) (yh, yl) =
  let zl = add xl yl and zh = add xh yh in
  ((if ucompare zl xl < 0 then succ zh else zh), zl)

let shl2 (xh, xl) n =
  assert (0 < n && n < size + size);
  if n < size
  then (logor (shift_left xh n) (shift_right_logical xl (size - n)),
        shift_left xl n)
  else (shift_left xl (n - size), 0n)

let mul2 x y =
  let halfsize = size / 2 in
  let halfmask = pred (shift_left 1n halfsize) in
  let xl = logand x halfmask and xh = shift_right_logical x halfsize in
  let yl = logand y halfmask and yh = shift_right_logical y halfsize in
  add2 (mul xh yh, 0n)
    (add2 (shl2 (0n, mul xl yh) halfsize)
       (add2 (shl2 (0n, mul xh yl) halfsize)
          (0n, mul xl yl)))

let ucompare2 (xh, xl) (yh, yl) =
  let c = ucompare xh yh in if c = 0 then ucompare xl yl else c

let validate d m p =
  let md = mul2 m d in
  let one2 = (0n, 1n) in
  let twoszp = shl2 one2 (size + p) in
  let twop1 = shl2 one2 (p + 1) in
  ucompare2 twoszp md < 0 && ucompare2 md (add2 twoszp twop1) <= 0
*)

let raise_symbol dbg symb =
  Cop(Craise Lambda.Raise_regular, [Cconst_symbol (symb, dbg)], dbg)

let rec div_int c1 c2 is_safe dbg =
  match (c1, c2) with
    (c1, Cconst_int (0, _)) ->
      Csequence(c1, raise_symbol dbg "caml_exn_Division_by_zero")
  | (c1, Cconst_int (1, _)) ->
      c1
  | (Cconst_int (n1, _), Cconst_int (n2, _)) ->
      Cconst_int (n1 / n2, dbg)
  | (c1, Cconst_int (n, _)) when n <> min_int ->
      let l = Misc.log2 n in
      if n = 1 lsl l then
        (* Algorithm:
              t = shift-right-signed(c1, l - 1)
              t = shift-right(t, W - l)
              t = c1 + t
              res = shift-right-signed(c1 + t, l)
        *)
        Cop(Casr, [bind "dividend" c1 (fun c1 ->
                     let t = asr_int c1 (Cconst_int (l - 1, dbg)) dbg in
                     let t =
                       lsr_int t (Cconst_int (Nativeint.size - l, dbg)) dbg
                     in
                     add_int c1 t dbg);
                   Cconst_int (l, dbg)], dbg)
      else if n < 0 then
        sub_int (Cconst_int (0, dbg))
          (div_int c1 (Cconst_int (-n, dbg)) is_safe dbg)
          dbg
      else begin
        let (m, p) = divimm_parameters (Nativeint.of_int n) in
        (* Algorithm:
              t = multiply-high-signed(c1, m)
              if m < 0, t = t + c1
              if p > 0, t = shift-right-signed(t, p)
              res = t + sign-bit(c1)
        *)
        bind "dividend" c1 (fun c1 ->
          let t = Cop(Cmulhi, [c1; natint_const_untagged dbg m], dbg) in
          let t = if m < 0n then Cop(Caddi, [t; c1], dbg) else t in
          let t =
            if p > 0 then Cop(Casr, [t; Cconst_int (p, dbg)], dbg) else t
          in
          add_int t (lsr_int c1 (Cconst_int (Nativeint.size - 1, dbg)) dbg) dbg)
      end
  | (c1, c2) when !Clflags.unsafe || is_safe = Lambda.Unsafe ->
      Cop(Cdivi, [c1; c2], dbg)
  | (c1, c2) ->
      bind "divisor" c2 (fun c2 ->
        bind "dividend" c1 (fun c1 ->
          Cifthenelse(c2,
                      dbg,
                      Cop(Cdivi, [c1; c2], dbg),
                      dbg,
                      raise_symbol dbg "caml_exn_Division_by_zero",
                      dbg)))

let mod_int c1 c2 is_safe dbg =
  match (c1, c2) with
    (c1, Cconst_int (0, _)) ->
      Csequence(c1, raise_symbol dbg "caml_exn_Division_by_zero")
  | (c1, Cconst_int ((1 | (-1)), _)) ->
      Csequence(c1, Cconst_int (0, dbg))
  | (Cconst_int (n1, _), Cconst_int (n2, _)) ->
      Cconst_int (n1 mod n2, dbg)
  | (c1, (Cconst_int (n, _) as c2)) when n <> min_int ->
      let l = Misc.log2 n in
      if n = 1 lsl l then
        (* Algorithm:
              t = shift-right-signed(c1, l - 1)
              t = shift-right(t, W - l)
              t = c1 + t
              t = bit-and(t, -n)
              res = c1 - t
         *)
        bind "dividend" c1 (fun c1 ->
          let t = asr_int c1 (Cconst_int (l - 1, dbg)) dbg in
          let t = lsr_int t (Cconst_int (Nativeint.size - l, dbg)) dbg in
          let t = add_int c1 t dbg in
          let t = Cop(Cand, [t; Cconst_int (-n, dbg)], dbg) in
          sub_int c1 t dbg)
      else
        bind "dividend" c1 (fun c1 ->
          sub_int c1 (mul_int (div_int c1 c2 is_safe dbg) c2 dbg) dbg)
  | (c1, c2) when !Clflags.unsafe || is_safe = Lambda.Unsafe ->
      (* Flambda already generates that test *)
      Cop(Cmodi, [c1; c2], dbg)
  | (c1, c2) ->
      bind "divisor" c2 (fun c2 ->
        bind "dividend" c1 (fun c1 ->
          Cifthenelse(c2,
                      dbg,
                      Cop(Cmodi, [c1; c2], dbg),
                      dbg,
                      raise_symbol dbg "caml_exn_Division_by_zero",
                      dbg)))

(* Division or modulo on boxed integers.  The overflow case min_int / -1
   can occur, in which case we force x / -1 = -x and x mod -1 = 0. (PR#5513). *)

let is_different_from x = function
    Cconst_int (n, _) -> n <> x
  | Cconst_natint (n, _) -> n <> Nativeint.of_int x
  | _ -> false

let safe_divmod_bi mkop is_safe mkm1 c1 c2 bi dbg =
  bind "divisor" c2 (fun c2 ->
  bind "dividend" c1 (fun c1 ->
    let c = mkop c1 c2 is_safe dbg in
    if Arch.division_crashes_on_overflow
    && bi <> Primitive.Pint32
    && not (is_different_from (-1) c2)
    then
      Cifthenelse(Cop(Ccmpi Cne, [c2; Cconst_int (-1, dbg)], dbg),
        dbg, c,
        dbg, mkm1 c1 dbg,
        dbg)
    else
      c))

let safe_div_bi is_safe =
  safe_divmod_bi div_int is_safe
    (fun c1 dbg -> Cop(Csubi, [Cconst_int (0, dbg); c1], dbg))

let safe_mod_bi is_safe =
  safe_divmod_bi mod_int is_safe (fun _ dbg -> Cconst_int (0, dbg))

(* Bool *)

let test_bool dbg cmm =
  match cmm with
  | Cop(Caddi, [Cop(Clsl, [c; Cconst_int (1, _)], _); Cconst_int (1, _)], _) ->
      c
  | Cconst_int (n, dbg) ->
      if n = 1 then
        Cconst_int (0, dbg)
      else
        Cconst_int (1, dbg)
  | c -> Cop(Ccmpi Cne, [c; Cconst_int (1, dbg)], dbg)

(* Float *)

let box_float dbg c = Cop(Calloc, [alloc_float_header dbg; c], dbg)

let unbox_float dbg =
  map_tail
    (function
      | Cop(Calloc, [Cconst_natint (hdr, _); c], _)
        when Nativeint.equal hdr float_header ->
          c
      | Cconst_symbol (s, _dbg) as cmm ->
          begin match Cmmgen_state.structured_constant_of_sym s with
          | Some (Uconst_float x) ->
              Cconst_float (x, dbg) (* or keep _dbg? *)
          | _ ->
              Cop(mk_load_immut Double, [cmm], dbg)
          end
      | cmm -> Cop(mk_load_immut Double, [cmm], dbg)
    )

(* Conversions for 16-bit floats *)

let float_of_float16 dbg c =
  Cop(Cextcall("caml_double_of_float16", typ_float, [XInt], false), [c], dbg)
let float16_of_float dbg c =
  Cop(Cextcall("caml_float16_of_double", typ_int, [XFloat], false), [c], dbg)

(* Complex *)

let box_complex dbg c_re c_im =
  Cop(Calloc, [alloc_floatarray_header 2 dbg; c_re; c_im], dbg)

let complex_re c dbg =
  Cop(mk_load_immut Double, [c], dbg)
let complex_im c dbg =
  Cop(mk_load_immut Double,
      [Cop(Cadda, [c; Cconst_int (size_float, dbg)], dbg)], dbg)

(* Unit *)

let return_unit dbg c = Csequence(c, Cconst_int (1, dbg))

let rec remove_unit = function
    Cconst_int (1, _) -> Ctuple []
  | Csequence(c, Cconst_int (1, _)) -> c
  | Csequence(c1, c2) ->
      Csequence(c1, remove_unit c2)
  | Cifthenelse(cond, ifso_dbg, ifso, ifnot_dbg, ifnot, dbg) ->
      Cifthenelse(cond,
        ifso_dbg, remove_unit ifso,
        ifnot_dbg,
        remove_unit ifnot, dbg)
  | Cswitch(sel, index, cases, dbg) ->
      Cswitch(sel, index,
        Array.map (fun (case, dbg) -> remove_unit case, dbg) cases,
        dbg)
  | Ccatch(rec_flag, handlers, body) ->
      let map_h (n, ids, handler, dbg) = (n, ids, remove_unit handler, dbg) in
      Ccatch(rec_flag, List.map map_h handlers, remove_unit body)
  | Ctrywith(body, exn, handler, dbg) ->
      Ctrywith(remove_unit body, exn, remove_unit handler, dbg)
  | Clet(id, c1, c2) ->
      Clet(id, c1, remove_unit c2)
  | Cop(Capply _mty, args, dbg) ->
      Cop(Capply typ_void, args, dbg)
  | Cop(Cextcall(proc, _ty_res, ty_args, alloc), args, dbg) ->
      Cop(Cextcall(proc, typ_void, ty_args, alloc), args, dbg)
  | Cexit (_,_) as c -> c
  | Ctuple [] as c -> c
  | c -> Csequence(c, Ctuple [])

let field_address ptr n dbg =
  if n = 0
  then ptr
  else Cop(Cadda, [ptr; Cconst_int(n * size_addr, dbg)], dbg)

let get_field_gen mutability ptr n dbg =
  Cop(Cload {memory_chunk=Word_val; mutability; is_atomic=false},
      [field_address ptr n dbg], dbg)

let get_field_codepointer mutability ptr n dbg =
  Cop(Cload {memory_chunk=Word_int; mutability; is_atomic=false},
      [field_address ptr n dbg], dbg)

let set_field ptr n newval init dbg =
  Cop(Cstore (Word_val, init), [field_address ptr n dbg; newval], dbg)

let get_header ptr dbg =
  (* Headers can be mutated when forcing a lazy value. However, for all
     purposes that the mutability tag currently serves in the compiler, header
     loads can be marked as [Immutable], since the runtime should ensure that
     there is no data race on headers. This saves performance with
     ThreadSanitizer instrumentation by avoiding to instrument header loads. *)
  Cop(
    mk_load_immut Word_int,
    [Cop(Cadda, [ptr; Cconst_int(-size_int, dbg)], dbg)], dbg)

let get_header_masked ptr dbg =
  if Config.reserved_header_bits > 0 then
    let header_mask = (1 lsl (64 - Config.reserved_header_bits)) - 1
    in Cop(Cand, [get_header ptr dbg; Cconst_int (header_mask, dbg)], dbg)
  else
    get_header ptr dbg

let tag_offset =
  if big_endian then -1 else -size_int

let get_tag ptr dbg =
  (* Same comment as [get_header] above *)
  Cop(
    mk_load_immut Byte_unsigned,
    [Cop(Cadda, [ptr; Cconst_int(tag_offset, dbg)], dbg)], dbg)

let get_size ptr dbg =
  Cop(Clsr, [get_header_masked ptr dbg; Cconst_int (10, dbg)], dbg)

(* Array indexing *)

let log2_size_addr = Misc.log2 size_addr
let log2_size_float = Misc.log2 size_float

let wordsize_shift = 9
let numfloat_shift = 9 + log2_size_float - log2_size_addr

let is_addr_array_hdr hdr dbg =
  Cop(Ccmpi Cne,
    [Cop(Cand, [hdr; Cconst_int (255, dbg)], dbg); floatarray_tag dbg],
    dbg)

let is_addr_array_ptr ptr dbg =
  Cop(Ccmpi Cne, [get_tag ptr dbg; floatarray_tag dbg], dbg)

let addr_array_length_shifted hdr dbg =
  Cop(Clsr, [hdr; Cconst_int (wordsize_shift, dbg)], dbg)
let float_array_length_shifted hdr dbg =
  Cop(Clsr, [hdr; Cconst_int (numfloat_shift, dbg)], dbg)

let lsl_const c n dbg =
  if n = 0 then c
  else Cop(Clsl, [c; Cconst_int (n, dbg)], dbg)

(* Produces a pointer to the element of the array [ptr] on the position [ofs]
   with the given element [log2size] log2 element size. [ofs] is given as a
   tagged int expression.
   The optional ?typ argument is the C-- type of the result.
   By default, it is Addr, meaning we are constructing a derived pointer
   into the heap.  If we know the pointer is outside the heap
   (this is the case for bigarray indexing), we give type Int instead. *)

let array_indexing ?typ log2size ptr ofs dbg =
  let add =
    match typ with
    | None | Some Addr -> Cadda
    | Some Int -> Caddi
    | _ -> assert false in
  match ofs with
  | Cconst_int (n, _) ->
      let i = n asr 1 in
      if i = 0 then ptr
      else Cop(add, [ptr; Cconst_int(i lsl log2size, dbg)], dbg)
  | Cop(Caddi,
        [Cop(Clsl, [c; Cconst_int (1, _)], _); Cconst_int (1, _)], dbg') ->
      Cop(add, [ptr; lsl_const c log2size dbg], dbg')
  | Cop(Caddi, [c; Cconst_int (n, _)], dbg') when log2size = 0 ->
      Cop(add,
        [Cop(add, [ptr; untag_int c dbg], dbg); Cconst_int (n asr 1, dbg)],
        dbg')
  | Cop(Caddi, [c; Cconst_int (n, _)], _) ->
      Cop(add, [Cop(add, [ptr; lsl_const c (log2size - 1) dbg], dbg);
                    Cconst_int((n-1) lsl (log2size - 1), dbg)], dbg)
  | _ when log2size = 0 ->
      Cop(add, [ptr; untag_int ofs dbg], dbg)
  | _ ->
      Cop(add, [Cop(add, [ptr; lsl_const ofs (log2size - 1) dbg], dbg);
                    Cconst_int((-1) lsl (log2size - 1), dbg)], dbg)

let addr_array_ref arr ofs dbg =
  Cop(mk_load_mut Word_val,
    [array_indexing log2_size_addr arr ofs dbg], dbg)
let int_array_ref arr ofs dbg =
  Cop(mk_load_mut Word_int,
    [array_indexing log2_size_addr arr ofs dbg], dbg)
let unboxed_float_array_ref arr ofs dbg =
  Cop(mk_load_mut Double,
    [array_indexing log2_size_float arr ofs dbg], dbg)
let float_array_ref arr ofs dbg =
  box_float dbg (unboxed_float_array_ref arr ofs dbg)

let addr_array_set arr ofs newval dbg =
  Cop(Cextcall("caml_modify", typ_void, [], false),
      [array_indexing log2_size_addr arr ofs dbg; newval], dbg)
let addr_array_initialize arr ofs newval dbg =
  Cop(Cextcall("caml_initialize", typ_void, [], false),
      [array_indexing log2_size_addr arr ofs dbg; newval], dbg)
let int_array_set arr ofs newval dbg =
  Cop(Cstore (Word_int, Lambda.Assignment),
    [array_indexing log2_size_addr arr ofs dbg; newval], dbg)
let float_array_set arr ofs newval dbg =
  Cop(Cstore (Double, Lambda.Assignment),
    [array_indexing log2_size_float arr ofs dbg; newval], dbg)

(* String length *)

(* Length of string block *)

let string_length exp dbg =
  bind "str" exp (fun str ->
    let tmp_var = V.create_local "tmp" in
    Clet(VP.create tmp_var,
         Cop(Csubi,
             [Cop(Clsl,
                   [get_size str dbg;
                     Cconst_int (log2_size_addr, dbg)],
                   dbg);
              Cconst_int (1, dbg)],
             dbg),
         Cop(Csubi,
             [Cvar tmp_var;
               Cop(mk_load_mut Byte_unsigned,
                     [Cop(Cadda, [str; Cvar tmp_var], dbg)], dbg)], dbg)))

let bigstring_length ba dbg =
  Cop(mk_load_mut Word_int, [field_address ba 5 dbg], dbg)

(* Message sending *)

let lookup_tag obj tag dbg =
  bind "tag" tag (fun tag ->
    Cop(Cextcall("caml_get_public_method", typ_val, [], false),
        [obj; tag],
        dbg))

let lookup_label obj lab dbg =
  bind "lab" lab (fun lab ->
    let table = Cop (mk_load_mut Word_val, [obj], dbg) in
    addr_array_ref table lab dbg)

let call_cached_method obj tag cache pos args dbg =
  let arity = List.length args in
  let cache = array_indexing log2_size_addr cache pos dbg in
  Compilenv.need_send_fun arity;
  Cop(Capply typ_val,
      Cconst_symbol("caml_send" ^ Int.to_string arity, dbg) ::
        obj :: tag :: cache :: args,
      dbg)

(* Allocation *)

let make_alloc_generic set_fn dbg tag wordsize args =
  if wordsize <= Config.max_young_wosize then
    Cop(Calloc, Cconst_natint(block_header tag wordsize, dbg) :: args, dbg)
  else begin
    let id = V.create_local "*alloc*" in
    let rec fill_fields idx = function
      [] -> Cvar id
    | e1::el -> Csequence(set_fn (Cvar id) (Cconst_int (idx, dbg)) e1 dbg,
                          fill_fields (idx + 2) el) in
    Clet(VP.create id,
         Cop(Cextcall("caml_alloc_shr_check_gc", typ_val, [], true),
                 [Cconst_int (wordsize, dbg); Cconst_int (tag, dbg)], dbg),
         fill_fields 1 args)
  end

let make_alloc dbg tag args =
  let addr_array_init arr ofs newval dbg =
    Cop(Cextcall("caml_initialize", typ_void, [], false),
        [array_indexing log2_size_addr arr ofs dbg; newval], dbg)
  in
  make_alloc_generic addr_array_init dbg tag (List.length args) args

let make_float_alloc dbg tag args =
  make_alloc_generic float_array_set dbg tag
                     (List.length args * size_float / size_addr) args

(* Bounds checking *)

let make_checkbound dbg = function
  | [Cop(Clsr, [a1; Cconst_int (n, _)], _); Cconst_int (m, _)]
    when (m lsl n) > n ->
      Cop(Ccheckbound, [a1; Cconst_int(m lsl n + 1 lsl n - 1, dbg)], dbg)
  | args ->
      Cop(Ccheckbound, args, dbg)

(* Record application and currying functions *)

let apply_function_sym n =
  Compilenv.need_apply_fun n; "caml_apply" ^ Int.to_string n
let curry_function_sym n =
  Compilenv.need_curry_fun n;
  if n >= 0
  then "caml_curry" ^ Int.to_string n
  else "caml_tuplify" ^ Int.to_string (-n)

(* Big arrays *)

let bigarray_elt_size : Lambda.bigarray_kind -> int = function
    Pbigarray_unknown -> assert false
  | Pbigarray_float16 -> 2
  | Pbigarray_float32 -> 4
  | Pbigarray_float64 -> 8
  | Pbigarray_sint8 -> 1
  | Pbigarray_uint8 -> 1
  | Pbigarray_sint16 -> 2
  | Pbigarray_uint16 -> 2
  | Pbigarray_int32 -> 4
  | Pbigarray_int64 -> 8
  | Pbigarray_caml_int -> size_int
  | Pbigarray_native_int -> size_int
  | Pbigarray_complex32 -> 8
  | Pbigarray_complex64 -> 16

(* Produces a pointer to the element of the bigarray [b] on the position
   [args].  [args] is given as a list of tagged int expressions, one per array
   dimension. *)
let bigarray_indexing unsafe elt_kind layout b args dbg =
  let check_ba_bound bound idx v =
    Csequence(make_checkbound dbg [bound;idx], v) in
  (* Validates the given multidimensional offset against the array bounds and
     transforms it into a one dimensional offset.  The offsets are expressions
     evaluating to tagged int. *)
  let rec ba_indexing dim_ofs delta_ofs = function
    [] -> assert false
  | [arg] ->
      if unsafe then arg
      else
        bind "idx" arg (fun idx ->
          (* Load the untagged int bound for the given dimension *)
          let bound =
            Cop(mk_load_mut Word_int,
                [field_address b dim_ofs dbg], dbg)
          in
          let idxn = untag_int idx dbg in
          check_ba_bound bound idxn idx)
  | arg1 :: argl ->
      (* The remainder of the list is transformed into a one dimensional offset
         *)
      let rem = ba_indexing (dim_ofs + delta_ofs) delta_ofs argl in
      (* Load the untagged int bound for the given dimension *)
      let bound =
        Cop(mk_load_mut Word_int,
            [field_address b dim_ofs dbg], dbg)
      in
      if unsafe then add_int (mul_int (decr_int rem dbg) bound dbg) arg1 dbg
      else
        bind "idx" arg1 (fun idx ->
          bind "bound" bound (fun bound ->
            let idxn = untag_int idx dbg in
            (* [offset = rem * (tag_int bound) + idx] *)
            let offset =
              add_int (mul_int (decr_int rem dbg) bound dbg) idx dbg
            in
            check_ba_bound bound idxn offset)) in
  (* The offset as an expression evaluating to int *)
  let offset =
    match (layout : Lambda.bigarray_layout) with
      Pbigarray_unknown_layout ->
        assert false
    | Pbigarray_c_layout ->
        ba_indexing (4 + List.length args) (-1) (List.rev args)
    | Pbigarray_fortran_layout ->
        ba_indexing 5 1
          (List.map (fun idx -> sub_int idx (Cconst_int (2, dbg)) dbg) args)
  and elt_size =
    bigarray_elt_size elt_kind in
  (* [array_indexing] can simplify the given expressions *)
  array_indexing ~typ:Addr (Misc.log2 elt_size)
                 (Cop(mk_load_mut Word_int,
                    [field_address b 1 dbg], dbg)) offset dbg

let bigarray_word_kind : Lambda.bigarray_kind -> memory_chunk = function
    Pbigarray_unknown -> assert false
  | Pbigarray_float16 -> Sixteen_unsigned
  | Pbigarray_float32 -> Single
  | Pbigarray_float64 -> Double
  | Pbigarray_sint8 -> Byte_signed
  | Pbigarray_uint8 -> Byte_unsigned
  | Pbigarray_sint16 -> Sixteen_signed
  | Pbigarray_uint16 -> Sixteen_unsigned
  | Pbigarray_int32 -> Thirtytwo_signed
  | Pbigarray_int64 -> Word_int
  | Pbigarray_caml_int -> Word_int
  | Pbigarray_native_int -> Word_int
  | Pbigarray_complex32 -> Single
  | Pbigarray_complex64 -> Double

let bigarray_get unsafe elt_kind layout b args dbg =
  bind "ba" b (fun b ->
    match (elt_kind : Lambda.bigarray_kind) with
      Pbigarray_complex32 | Pbigarray_complex64 ->
        let kind = bigarray_word_kind elt_kind in
        let sz = bigarray_elt_size elt_kind / 2 in
        bind "addr"
          (bigarray_indexing unsafe elt_kind layout b args dbg) (fun addr ->
            bind "reval"
              (Cop(mk_load_mut kind, [addr], dbg)) (fun reval ->
                bind "imval"
                  (Cop(mk_load_mut kind,
                       [Cop(Cadda, [addr; Cconst_int (sz, dbg)], dbg)], dbg))
                  (fun imval -> box_complex dbg reval imval)))
    | _ ->
        Cop(mk_load_mut (bigarray_word_kind elt_kind),
            [bigarray_indexing unsafe elt_kind layout b args dbg],
            dbg))

let bigarray_set unsafe elt_kind layout b args newval dbg =
  bind "ba" b (fun b ->
    match (elt_kind : Lambda.bigarray_kind) with
      Pbigarray_complex32 | Pbigarray_complex64 ->
        let kind = bigarray_word_kind elt_kind in
        let sz = bigarray_elt_size elt_kind / 2 in
        bind "newval" newval (fun newv ->
        bind "addr" (bigarray_indexing unsafe elt_kind layout b args dbg)
          (fun addr ->
          Csequence(
            Cop(Cstore (kind, Assignment), [addr; complex_re newv dbg], dbg),
            Cop(Cstore (kind, Assignment),
                [Cop(Cadda, [addr; Cconst_int (sz, dbg)], dbg);
                 complex_im newv dbg],
                dbg))))
    | _ ->
        Cop(Cstore (bigarray_word_kind elt_kind, Assignment),
            [bigarray_indexing unsafe elt_kind layout b args dbg; newval],
            dbg))

(* the three functions below assume 64-bit words *)
let () = assert (size_int = 8)

(* low_32 x is a value which agrees with x on at least the low 32 bits *)
let rec low_32 dbg = function
    (* Ignore sign and zero extensions, which do not affect the low bits *)
  | Cop(Casr, [Cop(Clsl, [x; Cconst_int (32, _)], _);
               Cconst_int (32, _)], _)
  | Cop(Cand, [x; Cconst_natint (0xFFFFFFFFn, _)], _) ->
    low_32 dbg x
  | Clet(id, e, body) ->
    Clet(id, e, low_32 dbg body)
  | x -> x

(* sign_extend_32 sign-extends values from 32 bits to the word size. *)
let sign_extend_32 dbg e =
  Cop(Casr, [Cop(Clsl, [low_32 dbg e; Cconst_int(32, dbg)], dbg);
             Cconst_int(32, dbg)], dbg)

(* zero_extend_32 zero-extends values from 32 bits to the word size. *)
let zero_extend_32 dbg e =
  Cop(Cand, [low_32 dbg e; natint_const_untagged dbg 0xFFFFFFFFn], dbg)

(* Boxed integers *)

let operations_boxed_int (bi : Primitive.boxed_integer) =
  match bi with
    Pnativeint -> caml_nativeint_ops
  | Pint32 -> caml_int32_ops
  | Pint64 -> caml_int64_ops

let alloc_header_boxed_int (bi : Primitive.boxed_integer) =
  match bi with
    Pnativeint -> alloc_boxedintnat_header
  | Pint32 -> alloc_boxedint32_header
  | Pint64 -> alloc_boxedint64_header

let box_int_gen dbg (bi : Primitive.boxed_integer) arg =
  let arg' =
    if bi = Primitive.Pint32 then
      if big_endian
      then Cop(Clsl, [arg; Cconst_int (32, dbg)], dbg)
      else sign_extend_32 dbg arg
    else arg
  in
  Cop(Calloc, [alloc_header_boxed_int bi dbg;
               Cconst_symbol(operations_boxed_int bi, dbg);
               arg'], dbg)

let alloc_matches_boxed_int bi ~hdr ~ops =
  match (bi : Primitive.boxed_integer), hdr, ops with
  | Pnativeint, Cconst_natint (hdr, _dbg), Cconst_symbol (sym, _) ->
      Nativeint.equal hdr boxedintnat_header
        && String.equal sym caml_nativeint_ops
  | Pint32, Cconst_natint (hdr, _dbg), Cconst_symbol (sym, _) ->
      Nativeint.equal hdr boxedint32_header
        && String.equal sym caml_int32_ops
  | Pint64, Cconst_natint (hdr, _dbg), Cconst_symbol (sym, _) ->
      Nativeint.equal hdr boxedint64_header
        && String.equal sym caml_int64_ops
  | (Pnativeint | Pint32 | Pint64), _, _ -> false

let unbox_int dbg bi =
  let default arg =
    let memory_chunk = if bi = Primitive.Pint32
      then Thirtytwo_signed else Word_int
    in
    Cop(
      mk_load_immut memory_chunk,
      [Cop(Cadda, [arg; Cconst_int (size_addr, dbg)], dbg)], dbg)
  in
  map_tail
    (function
      | Cop(Calloc,
            [hdr; ops;
             Cop(Clsl, [contents; Cconst_int (32, _)], _dbg')], _dbg)
        when bi = Primitive.Pint32 && big_endian
             && alloc_matches_boxed_int bi ~hdr ~ops ->
          (* Force sign-extension of low 32 bits *)
          sign_extend_32 dbg contents
      | Cop(Calloc,
            [hdr; ops; contents], _dbg)
        when bi = Primitive.Pint32 && not big_endian
             && alloc_matches_boxed_int bi ~hdr ~ops ->
          (* Force sign-extension of low 32 bits *)
          sign_extend_32 dbg contents
      | Cop(Calloc, [hdr; ops; contents], _dbg)
        when alloc_matches_boxed_int bi ~hdr ~ops ->
          contents
      | Cconst_symbol (s, _dbg) as cmm ->
          begin match Cmmgen_state.structured_constant_of_sym s, bi with
          | Some (Uconst_nativeint n), Primitive.Pnativeint ->
              natint_const_untagged dbg n
          | Some (Uconst_int32 n), Primitive.Pint32 ->
              natint_const_untagged dbg (Nativeint.of_int32 n)
          | Some (Uconst_int64 n), Primitive.Pint64 ->
              natint_const_untagged dbg (Int64.to_nativeint n)
          | _ ->
              default cmm
          end
      | cmm ->
          default cmm
    )

let make_unsigned_int bi arg dbg =
  if bi = Primitive.Pint32
  then zero_extend_32 dbg arg
  else arg

let unaligned_load_16 ptr idx dbg =
  if Arch.allow_unaligned_access
  then Cop(mk_load_mut Sixteen_unsigned, [add_int ptr idx dbg], dbg)
  else
    let cconst_int i = Cconst_int (i, dbg) in
    let v1 = Cop(mk_load_mut Byte_unsigned, [add_int ptr idx dbg], dbg) in
    let v2 = Cop(mk_load_mut Byte_unsigned,
                 [add_int (add_int ptr idx dbg) (cconst_int 1) dbg], dbg) in
    let b1, b2 = if Arch.big_endian then v1, v2 else v2, v1 in
    Cop(Cor, [lsl_int b1 (cconst_int 8) dbg; b2], dbg)

let unaligned_set_16 ptr idx newval dbg =
  if Arch.allow_unaligned_access
  then
    Cop(Cstore (Sixteen_unsigned, Assignment),
      [add_int ptr idx dbg; newval], dbg)
  else
    let cconst_int i = Cconst_int (i, dbg) in
    let v1 =
      Cop(Cand, [Cop(Clsr, [newval; cconst_int 8], dbg);
        cconst_int 0xFF], dbg)
    in
    let v2 = Cop(Cand, [newval; cconst_int 0xFF], dbg) in
    let b1, b2 = if Arch.big_endian then v1, v2 else v2, v1 in
    Csequence(
        Cop(Cstore (Byte_unsigned, Assignment), [add_int ptr idx dbg; b1], dbg),
        Cop(Cstore (Byte_unsigned, Assignment),
            [add_int (add_int ptr idx dbg) (cconst_int 1) dbg; b2], dbg))

let unaligned_load_32 ptr idx dbg =
  if Arch.allow_unaligned_access
  then Cop(mk_load_mut Thirtytwo_unsigned, [add_int ptr idx dbg], dbg)
  else
    let cconst_int i = Cconst_int (i, dbg) in
    let v1 = Cop(mk_load_mut Byte_unsigned, [add_int ptr idx dbg], dbg) in
    let v2 = Cop(mk_load_mut Byte_unsigned,
                 [add_int (add_int ptr idx dbg) (cconst_int 1) dbg], dbg)
    in
    let v3 = Cop(mk_load_mut Byte_unsigned,
                 [add_int (add_int ptr idx dbg) (cconst_int 2) dbg], dbg)
    in
    let v4 = Cop(mk_load_mut Byte_unsigned,
                 [add_int (add_int ptr idx dbg) (cconst_int 3) dbg], dbg)
    in
    let b1, b2, b3, b4 =
      if Arch.big_endian
      then v1, v2, v3, v4
      else v4, v3, v2, v1 in
    Cop(Cor,
      [Cop(Cor, [lsl_int b1 (cconst_int 24) dbg;
         lsl_int b2 (cconst_int 16) dbg], dbg);
       Cop(Cor, [lsl_int b3 (cconst_int 8) dbg; b4], dbg)],
      dbg)

let unaligned_set_32 ptr idx newval dbg =
  if Arch.allow_unaligned_access
  then
    Cop(Cstore (Thirtytwo_unsigned, Assignment), [add_int ptr idx dbg; newval],
      dbg)
  else
    let cconst_int i = Cconst_int (i, dbg) in
    let v1 =
      Cop(Cand, [Cop(Clsr, [newval; cconst_int 24], dbg); cconst_int 0xFF], dbg)
    in
    let v2 =
      Cop(Cand, [Cop(Clsr, [newval; cconst_int 16], dbg); cconst_int 0xFF], dbg)
    in
    let v3 =
      Cop(Cand, [Cop(Clsr, [newval; cconst_int 8], dbg); cconst_int 0xFF], dbg)
    in
    let v4 = Cop(Cand, [newval; cconst_int 0xFF], dbg) in
    let b1, b2, b3, b4 =
      if Arch.big_endian
      then v1, v2, v3, v4
      else v4, v3, v2, v1 in
    Csequence(
        Csequence(
            Cop(Cstore (Byte_unsigned, Assignment),
                [add_int ptr idx dbg; b1], dbg),
            Cop(Cstore (Byte_unsigned, Assignment),
                [add_int (add_int ptr idx dbg) (cconst_int 1) dbg; b2],
                dbg)),
        Csequence(
            Cop(Cstore (Byte_unsigned, Assignment),
                [add_int (add_int ptr idx dbg) (cconst_int 2) dbg; b3],
                dbg),
            Cop(Cstore (Byte_unsigned, Assignment),
                [add_int (add_int ptr idx dbg) (cconst_int 3) dbg; b4],
                dbg)))

let unaligned_load_64 ptr idx dbg =
  if Arch.allow_unaligned_access
  then Cop(mk_load_mut Word_int, [add_int ptr idx dbg], dbg)
  else
    let cconst_int i = Cconst_int (i, dbg) in
    let v1 = Cop(mk_load_mut Byte_unsigned, [add_int ptr idx dbg], dbg) in
    let v2 = Cop(mk_load_mut Byte_unsigned,
                 [add_int (add_int ptr idx dbg) (cconst_int 1) dbg], dbg) in
    let v3 = Cop(mk_load_mut Byte_unsigned,
                 [add_int (add_int ptr idx dbg) (cconst_int 2) dbg], dbg) in
    let v4 = Cop(mk_load_mut Byte_unsigned,
                 [add_int (add_int ptr idx dbg) (cconst_int 3) dbg], dbg) in
    let v5 = Cop(mk_load_mut Byte_unsigned,
                 [add_int (add_int ptr idx dbg) (cconst_int 4) dbg], dbg) in
    let v6 = Cop(mk_load_mut Byte_unsigned,
                 [add_int (add_int ptr idx dbg) (cconst_int 5) dbg], dbg) in
    let v7 = Cop(mk_load_mut Byte_unsigned,
                 [add_int (add_int ptr idx dbg) (cconst_int 6) dbg], dbg) in
    let v8 = Cop(mk_load_mut Byte_unsigned,
                 [add_int (add_int ptr idx dbg) (cconst_int 7) dbg], dbg) in
    let b1, b2, b3, b4, b5, b6, b7, b8 =
      if Arch.big_endian
      then v1, v2, v3, v4, v5, v6, v7, v8
      else v8, v7, v6, v5, v4, v3, v2, v1 in
    Cop(Cor,
        [Cop(Cor,
             [Cop(Cor, [lsl_int b1 (cconst_int (8*7)) dbg;
                        lsl_int b2 (cconst_int (8*6)) dbg], dbg);
              Cop(Cor, [lsl_int b3 (cconst_int (8*5)) dbg;
                        lsl_int b4 (cconst_int (8*4)) dbg], dbg)],
             dbg);
         Cop(Cor,
             [Cop(Cor, [lsl_int b5 (cconst_int (8*3)) dbg;
                        lsl_int b6 (cconst_int (8*2)) dbg], dbg);
              Cop(Cor, [lsl_int b7 (cconst_int 8) dbg;
                        b8], dbg)],
             dbg)], dbg)

let unaligned_set_64 ptr idx newval dbg =
  if Arch.allow_unaligned_access
  then Cop(Cstore (Word_int, Assignment), [add_int ptr idx dbg; newval], dbg)
  else
    let cconst_int i = Cconst_int (i, dbg) in
    let v1 =
      Cop(Cand, [Cop(Clsr, [newval; cconst_int (8*7)], dbg); cconst_int 0xFF],
        dbg)
    in
    let v2 =
      Cop(Cand, [Cop(Clsr, [newval; cconst_int (8*6)], dbg); cconst_int 0xFF],
        dbg)
    in
    let v3 =
      Cop(Cand, [Cop(Clsr, [newval; cconst_int (8*5)], dbg); cconst_int 0xFF],
        dbg)
    in
    let v4 =
      Cop(Cand, [Cop(Clsr, [newval; cconst_int (8*4)], dbg); cconst_int 0xFF],
        dbg)
    in
    let v5 =
      Cop(Cand, [Cop(Clsr, [newval; cconst_int (8*3)], dbg); cconst_int 0xFF],
        dbg)
    in
    let v6 =
      Cop(Cand, [Cop(Clsr, [newval; cconst_int (8*2)], dbg); cconst_int 0xFF],
        dbg)
    in
    let v7 =
      Cop(Cand, [Cop(Clsr, [newval; cconst_int 8], dbg); cconst_int 0xFF],
        dbg)
    in
    let v8 = Cop(Cand, [newval; cconst_int 0xFF], dbg) in
    let b1, b2, b3, b4, b5, b6, b7, b8 =
      if Arch.big_endian
      then v1, v2, v3, v4, v5, v6, v7, v8
      else v8, v7, v6, v5, v4, v3, v2, v1 in
    Csequence(
        Csequence(
            Csequence(
                Cop(Cstore (Byte_unsigned, Assignment),
                    [add_int ptr idx dbg; b1],
                    dbg),
                Cop(Cstore (Byte_unsigned, Assignment),
                    [add_int (add_int ptr idx dbg) (cconst_int 1) dbg; b2],
                    dbg)),
            Csequence(
                Cop(Cstore (Byte_unsigned, Assignment),
                    [add_int (add_int ptr idx dbg) (cconst_int 2) dbg; b3],
                    dbg),
                Cop(Cstore (Byte_unsigned, Assignment),
                    [add_int (add_int ptr idx dbg) (cconst_int 3) dbg; b4],
                    dbg))),
        Csequence(
            Csequence(
                Cop(Cstore (Byte_unsigned, Assignment),
                    [add_int (add_int ptr idx dbg) (cconst_int 4) dbg; b5],
                    dbg),
                Cop(Cstore (Byte_unsigned, Assignment),
                    [add_int (add_int ptr idx dbg) (cconst_int 5) dbg; b6],
                    dbg)),
            Csequence(
                Cop(Cstore (Byte_unsigned, Assignment),
                    [add_int (add_int ptr idx dbg) (cconst_int 6) dbg; b7],
                    dbg),
                Cop(Cstore (Byte_unsigned, Assignment),
                    [add_int (add_int ptr idx dbg) (cconst_int 7) dbg; b8],
                    dbg))))

let max_or_zero a dbg =
  bind "size" a (fun a ->
    (* equivalent to
       Cifthenelse(Cop(Ccmpi Cle, [a; cconst_int 0]), cconst_int 0, a)

       if a is positive, sign is 0 hence sign_negation is full of 1
                         so sign_negation&a = a
       if a is negative, sign is full of 1 hence sign_negation is 0
                         so sign_negation&a = 0 *)
    let sign = Cop(Casr, [a; Cconst_int (size_int * 8 - 1, dbg)], dbg) in
    let sign_negation = Cop(Cxor, [sign; Cconst_int (-1, dbg)], dbg) in
    Cop(Cand, [sign_negation; a], dbg))

let check_bound safety access_size dbg length a2 k =
  match (safety : Lambda.is_safe) with
  | Unsafe -> k
  | Safe ->
      let offset =
        match (access_size : Clambda_primitives.memory_access_size) with
        | Sixteen -> 1
        | Thirty_two -> 3
        | Sixty_four -> 7
      in
      let a1 =
        sub_int length (Cconst_int (offset, dbg)) dbg
      in
      Csequence(make_checkbound dbg [max_or_zero a1 dbg; a2], k)

let opaque e dbg =
  Cop(Copaque, [e], dbg)

let unaligned_set size ptr idx newval dbg =
  match (size : Clambda_primitives.memory_access_size) with
  | Sixteen -> unaligned_set_16 ptr idx newval dbg
  | Thirty_two -> unaligned_set_32 ptr idx newval dbg
  | Sixty_four -> unaligned_set_64 ptr idx newval dbg

let unaligned_load size ptr idx dbg =
  match (size : Clambda_primitives.memory_access_size) with
  | Sixteen -> unaligned_load_16 ptr idx dbg
  | Thirty_two -> unaligned_load_32 ptr idx dbg
  | Sixty_four -> unaligned_load_64 ptr idx dbg

let box_sized size dbg exp =
  match (size : Clambda_primitives.memory_access_size) with
  | Sixteen -> tag_int exp dbg
  | Thirty_two -> box_int_gen dbg Pint32 exp
  | Sixty_four -> box_int_gen dbg Pint64 exp

(* Simplification of some primitives into C calls *)

let default_prim name =
  Primitive.simple ~name ~arity:0(*ignored*) ~alloc:true

let simplif_primitive p : Clambda_primitives.primitive =
  match (p : Clambda_primitives.primitive) with
  | Pduprecord _ ->
      Pccall (default_prim "caml_obj_dup")
  | Pbigarrayref(_unsafe, n, Pbigarray_unknown, _layout) ->
      Pccall (default_prim ("caml_ba_get_" ^ string_of_int n))
  | Pbigarrayset(_unsafe, n, Pbigarray_unknown, _layout) ->
      Pccall (default_prim ("caml_ba_set_" ^ string_of_int n))
  | Pbigarrayref(_unsafe, n, _kind, Pbigarray_unknown_layout) ->
      Pccall (default_prim ("caml_ba_get_" ^ string_of_int n))
  | Pbigarrayset(_unsafe, n, _kind, Pbigarray_unknown_layout) ->
      Pccall (default_prim ("caml_ba_set_" ^ string_of_int n))
  | p ->
      p

(* Build switchers both for constants and blocks *)

let transl_isout h arg dbg = tag_int (Cop(Ccmpa Clt, [h ; arg], dbg)) dbg

(* Build an actual switch (ie jump table) *)

let make_switch arg cases actions dbg =
  let extract_uconstant =
    function
    (* Constant integers loaded from a table should end in 1,
       so that Cload never produces untagged integers *)
    | Cconst_int     (n, _), _dbg when (n land 1) = 1 ->
        Some (Cint (Nativeint.of_int n))
    | Cconst_natint     (n, _), _dbg
      when Nativeint.(to_int (logand n one) = 1) ->
        Some (Cint n)
    | Cconst_symbol (s,_), _dbg ->
        Some (Csymbol_address s)
    | _ -> None
  in
  let extract_affine ~cases ~const_actions =
    let length = Array.length cases in
    if length >= 2
    then begin
      match const_actions.(cases.(0)), const_actions.(cases.(1)) with
      | Cint v0, Cint v1 ->
          let slope = Nativeint.sub v1 v0 in
          let check i = function
            | Cint v -> v = Nativeint.(add (mul (of_int i) slope) v0)
            | _ -> false
          in
          if Misc.Stdlib.Array.for_alli
              (fun i idx -> check i const_actions.(idx)) cases
          then Some (v0, slope)
          else None
      | _, _ ->
          None
    end
    else None
  in
  let make_table_lookup ~cases ~const_actions arg dbg =
    let table = Compilenv.new_const_symbol () in
    Cmmgen_state.add_constant table (Const_table (Local,
        Array.to_list (Array.map (fun act ->
          const_actions.(act)) cases)));
    addr_array_ref (Cconst_symbol (table, dbg)) (tag_int arg dbg) dbg
  in
  let make_affine_computation ~offset ~slope arg dbg =
    (* In case the resulting integers are an affine function of the index, we
       don't emit a table, and just compute the result directly *)
    add_int
      (mul_int arg (natint_const_untagged dbg slope) dbg)
      (natint_const_untagged dbg offset)
      dbg
  in
  match Misc.Stdlib.Array.all_somes (Array.map extract_uconstant actions) with
  | None ->
      Cswitch (arg,cases,actions,dbg)
  | Some const_actions ->
      match extract_affine ~cases ~const_actions with
      | Some (offset, slope) ->
          make_affine_computation ~offset ~slope arg dbg
      | None -> make_table_lookup ~cases ~const_actions arg dbg

module SArgBlocks =
struct
  type primitive = operation

  let eqint = Ccmpi Ceq
  let neint = Ccmpi Cne
  let leint = Ccmpi Cle
  let ltint = Ccmpi Clt
  let geint = Ccmpi Cge
  let gtint = Ccmpi Cgt

  type loc = Debuginfo.t
  type arg = expression
  type test = expression
  type act = expression

  (* CR mshinwell: GPR#2294 will fix the Debuginfo here *)

  let make_const i =  Cconst_int (i, Debuginfo.none)
  let make_prim p args = Cop (p,args, Debuginfo.none)
  let make_offset arg n = add_const arg n Debuginfo.none
  let make_isout h arg = Cop (Ccmpa Clt, [h ; arg], Debuginfo.none)
  let make_isin h arg = Cop (Ccmpa Cge, [h ; arg], Debuginfo.none)
  let make_is_nonzero arg = arg
  let arg_as_test arg = arg
  let make_if cond ifso ifnot =
    Cifthenelse (cond, Debuginfo.none, ifso, Debuginfo.none, ifnot,
      Debuginfo.none)
  let make_switch dbg arg cases actions =
    let actions = Array.map (fun expr -> expr, dbg) actions in
    make_switch arg cases actions dbg
  let bind arg body = bind "switcher" arg body

  let make_catch handler = match handler with
  | Cexit (i,[]) -> i,fun e -> e
  | _ ->
      let dbg = Debuginfo.none in
      let i = Lambda.next_raise_count () in
(*
      Printf.eprintf  "SHARE CMM: %i\n" i ;
      Printcmm.expression Format.str_formatter handler ;
      Printf.eprintf "%s\n" (Format.flush_str_formatter ()) ;
*)
      i,
      (fun body -> match body with
      | Cexit (j,_) ->
          if i=j then handler
          else body
      | _ ->  ccatch (i,[],body,handler, dbg))

  let make_exit i = Cexit (i,[])

end

(* cmm store, as sharing as normally been detected in previous
   phases, we only share exits *)
(* Some specific patterns can lead to switches where several cases
   point to the same action, but this action is not an exit (see GPR#1370).
   The addition of the index in the action array as context allows to
   share them correctly without duplication. *)
module StoreExpForSwitch =
  Switch.CtxStore
    (struct
      type t = expression
      type key = int option * int
      type context = int
      let make_key index expr =
        let continuation =
          match expr with
          | Cexit (i,[]) -> Some i
          | _ -> None
        in
        Some (continuation, index)
      let compare_key (cont, index) (cont', index') =
        match cont, cont' with
        | Some i, Some i' when i = i' -> 0
        | _, _ -> Stdlib.compare index index'
    end)

(* For string switches, we can use a generic store *)
module StoreExp =
  Switch.Store
    (struct
      type t = expression
      type key = int
      let make_key = function
        | Cexit (i,[]) -> Some i
        | _ -> None
      let compare_key = Stdlib.compare
    end)

module SwitcherBlocks = Switch.Make(SArgBlocks)

(* Int switcher, arg in [low..high],
   cases is list of individual cases, and is sorted by first component *)

let transl_int_switch dbg arg low high cases default = match cases with
| [] -> assert false
| _::_ ->
    let store = StoreExp.mk_store () in
    assert (store.Switch.act_store () default = 0) ;
    let cases =
      List.map
        (fun (i,act) -> i,store.Switch.act_store () act)
        cases in
    let rec inters plow phigh pact = function
      | [] ->
          if phigh = high then [plow,phigh,pact]
          else [(plow,phigh,pact); (phigh+1,high,0) ]
      | (i,act)::rem ->
          if i = phigh+1 then
            if pact = act then
              inters plow i pact rem
            else
              (plow,phigh,pact)::inters i i act rem
          else (* insert default *)
            if pact = 0 then
              if act = 0 then
                inters plow i 0 rem
              else
                (plow,i-1,pact)::
                inters i i act rem
            else (* pact <> 0 *)
              (plow,phigh,pact)::
              begin
                if act = 0 then inters (phigh+1) i 0 rem
                else (phigh+1,i-1,0)::inters i i act rem
              end in
    let inters = match cases with
    | [] -> assert false
    | (k0,act0)::rem ->
        if k0 = low then inters k0 k0 act0 rem
        else inters low (k0-1) 0 cases in
    bind "switcher" arg
      (fun a ->
        SwitcherBlocks.zyva
          dbg
          (low,high)
          a
          (Array.of_list inters) store)


let transl_switch_clambda loc arg index cases =
  let store = StoreExpForSwitch.mk_store () in
  let index =
    Array.map
      (fun j -> store.Switch.act_store j cases.(j))
      index in
  let n_index = Array.length index in
  let inters = ref []
  and this_high = ref (n_index-1)
  and this_low = ref (n_index-1)
  and this_act = ref index.(n_index-1) in
  for i = n_index-2 downto 0 do
    let act = index.(i) in
    if act = !this_act then
      decr this_low
    else begin
      inters := (!this_low, !this_high, !this_act) :: !inters ;
      this_high := i ;
      this_low := i ;
      this_act := act
    end
  done ;
  inters := (0, !this_high, !this_act) :: !inters ;
  match !inters with
  | [_] -> cases.(0)
  | inters ->
      bind "switcher" arg
        (fun a ->
           SwitcherBlocks.zyva
             loc
             (0,n_index-1)
             a
             (Array.of_list inters) store)

let strmatch_compile =
  let module S =
    Strmatch.Make
      (struct
        let string_block_length ptr = get_size ptr Debuginfo.none
        let transl_switch = transl_int_switch
      end) in
  S.compile

let ptr_offset ptr offset dbg =
  if offset = 0
  then ptr
  else Cop(Caddv, [ptr; Cconst_int(offset * size_addr, dbg)], dbg)

let direct_apply lbl args dbg =
  Cop(Capply typ_val, Cconst_symbol (lbl, dbg) :: args, dbg)

let generic_apply mut clos args dbg =
  match args with
  | [arg] ->
      bind "fun" clos (fun clos ->
        Cop(Capply typ_val, [get_field_codepointer mut clos 0 dbg; arg; clos],
          dbg))
  | _ ->
      let arity = List.length args in
      let cargs =
        Cconst_symbol(apply_function_sym arity, dbg) :: args @ [clos]
      in
      Cop(Capply typ_val, cargs, dbg)

let send kind met obj args dbg =
  let call_met obj args clos =
    (* met is never a simple expression, so it never gets turned into an
       Immutable load *)
    generic_apply Asttypes.Mutable clos (obj :: args) dbg
  in
  bind "obj" obj (fun obj ->
      match (kind : Lambda.meth_kind), args with
        Self, _ ->
          bind "met" (lookup_label obj met dbg)
            (call_met obj args)
      | Cached, cache :: pos :: args ->
          call_cached_method obj met cache pos args dbg
      | _ ->
          bind "met" (lookup_tag obj met dbg)
            (call_met obj args))

(*
CAMLprim value caml_cache_public_method (value meths, value tag, value *cache)
{
  int li = 3, hi = Field(meths,0), mi;
  while (li < hi) { // no need to check the 1st time
    mi = ((li+hi) >> 1) | 1;
    if (tag < Field(meths,mi)) hi = mi-2;
    else li = mi;
  }
  *cache = (li-3)*sizeof(value)+1;
  return Field (meths, li-1);
}
*)

let cache_public_method meths tag cache dbg =
  let raise_num = Lambda.next_raise_count () in
  let cconst_int i = Cconst_int (i, dbg) in
  let li = V.create_local "*li*" and hi = V.create_local "*hi*"
  and mi = V.create_local "*mi*" and tagged = V.create_local "*tagged*" in
  Clet_mut (
  VP.create li, typ_int, cconst_int 3,
  Clet_mut (
  VP.create hi, typ_int, Cop(mk_load_mut Word_int, [meths], dbg),
  Csequence(
  ccatch
    (raise_num, [],
     create_loop
       (Clet(
        VP.create mi,
        Cop(Cor,
            [Cop(Clsr, [Cop(Caddi, [Cvar li; Cvar hi], dbg); cconst_int 1],
               dbg);
             cconst_int 1],
            dbg),
        Csequence(
        Cifthenelse
          (Cop (Ccmpi Clt,
                [tag;
                 Cop(mk_load_mut Word_int,
                     [Cop(Cadda,
                          [meths; lsl_const (Cvar mi) log2_size_addr dbg],
                          dbg)],
                     dbg)], dbg),
           dbg, Cassign(hi, Cop(Csubi, [Cvar mi; cconst_int 2], dbg)),
           dbg, Cassign(li, Cvar mi),
           dbg),
        Cifthenelse
          (Cop(Ccmpi Cge, [Cvar li; Cvar hi], dbg),
           dbg, Cexit (raise_num, []),
           dbg, Ctuple [],
           dbg))))
       dbg,
     Ctuple [],
     dbg),
  Clet (
    VP.create tagged,
      Cop(Caddi, [lsl_const (Cvar li) log2_size_addr dbg;
        cconst_int(1 - 3 * size_addr)], dbg),
    Csequence(Cop (Cstore (Word_int, Assignment), [cache; Cvar tagged], dbg),
              Cvar tagged)))))

(* CR mshinwell: These will be filled in by later pull requests. *)
let placeholder_dbg () = Debuginfo.none
let placeholder_fun_dbg ~human_name:_ = Debuginfo.none

(* Generate an application function:
     (defun caml_applyN (a1 ... aN clos)
       (if (= clos.arity N)
         (app clos.direct a1 ... aN clos)
         (let (clos1 (app clos.code a1 clos)
               clos2 (app clos1.code a2 clos)
               ...
               closN-1 (app closN-2.code aN-1 closN-2))
           (app closN-1.code aN closN-1))))
*)

let apply_function_body arity =
  let dbg = placeholder_dbg in
  let arg = Array.make arity (V.create_local "arg") in
  for i = 1 to arity - 1 do arg.(i) <- V.create_local "arg" done;
  let clos = V.create_local "clos" in
  let rec app_fun clos n =
    if n = arity-1 then
      Cop(Capply typ_val,
          [get_field_codepointer Asttypes.Mutable (Cvar clos) 0 (dbg ());
           Cvar arg.(n);
           Cvar clos],
          dbg ())
    else begin
      let newclos = V.create_local "clos" in
      Clet(VP.create newclos,
           Cop(Capply typ_val,
               [get_field_codepointer Asttypes.Mutable (Cvar clos) 0 (dbg ());
                Cvar arg.(n); Cvar clos], dbg ()),
           app_fun newclos (n+1))
    end in
  let args = Array.to_list arg in
  let all_args = args @ [clos] in
  (args, clos,
   if arity = 1 then app_fun clos 0 else
   Cifthenelse(
   Cop(Ccmpi Ceq, [Cop(Casr,
                       [get_field_gen Asttypes.Mutable (Cvar clos) 1 (dbg());
                        Cconst_int(pos_arity_in_closinfo, dbg())], dbg());
                   Cconst_int(arity, dbg())], dbg()),
   dbg (),
   Cop(Capply typ_val,
       get_field_codepointer Asttypes.Mutable (Cvar clos) 2 (dbg ())
       :: List.map (fun s -> Cvar s) all_args,
       dbg ()),
   dbg (),
   app_fun clos 0,
   dbg ()))

let send_function arity =
  let dbg = placeholder_dbg in
  let cconst_int i = Cconst_int (i, dbg ()) in
  let (args, clos', body) = apply_function_body (1+arity) in
  let cache = V.create_local "cache"
  and obj = List.hd args
  and tag = V.create_local "tag" in
  let clos =
    let cache = Cvar cache and obj = Cvar obj and tag = Cvar tag in
    let meths = V.create_local "meths" and cached = V.create_local "cached" in
    let real = V.create_local "real" in
    let mask = get_field_gen Asttypes.Mutable (Cvar meths) 1 (dbg ()) in
    let cached_pos = Cvar cached in
    let tag_pos = Cop(Cadda, [Cop (Cadda, [cached_pos; Cvar meths], dbg ());
                              cconst_int(3*size_addr-1)], dbg ()) in
    let tag' = Cop(mk_load_mut Word_int, [tag_pos], dbg ()) in
    Clet (
    VP.create meths, Cop(mk_load_mut Word_val, [obj], dbg ()),
    Clet (
    VP.create cached,
      Cop(Cand, [Cop(mk_load_mut Word_int, [cache], dbg ()); mask],
          dbg ()),
    Clet (
    VP.create real,
    Cifthenelse(Cop(Ccmpa Cne, [tag'; tag], dbg ()),
                dbg (),
                cache_public_method (Cvar meths) tag cache (dbg ()),
                dbg (),
                cached_pos,
                dbg ()),
    Cop(mk_load_mut Word_val,
      [Cop(Cadda, [Cop (Cadda, [Cvar real; Cvar meths], dbg ());
       cconst_int(2*size_addr-1)], dbg ())], dbg ()))))

  in
  let body = Clet(VP.create clos', clos, body) in
  let cache = cache in
  let fun_name = "caml_send" ^ Int.to_string arity in
  let fun_args =
    [obj, typ_val; tag, typ_int; cache, typ_addr]
    @ List.map (fun id -> (id, typ_val)) (List.tl args) in
  let fun_dbg = placeholder_fun_dbg ~human_name:fun_name in
  Cfunction
   {fun_name;
    fun_args = List.map (fun (arg, ty) -> VP.create arg, ty) fun_args;
    fun_body = body;
    fun_codegen_options = [];
    fun_poll = Default_poll;
    fun_dbg;
   }

let apply_function arity =
  let (args, clos, body) = apply_function_body arity in
  let all_args = args @ [clos] in
  let fun_name = "caml_apply" ^ Int.to_string arity in
  let fun_dbg = placeholder_fun_dbg ~human_name:fun_name in
  Cfunction
   {fun_name;
    fun_args = List.map (fun arg -> (VP.create arg, typ_val)) all_args;
    fun_body = body;
    fun_codegen_options = [];
    fun_poll = Default_poll;
    fun_dbg;
   }

(* Generate tuplifying functions:
      (defun caml_tuplifyN (arg clos)
        (app clos.direct #0(arg) ... #N-1(arg) clos)) *)

let tuplify_function arity =
  let dbg = placeholder_dbg in
  let arg = V.create_local "arg" in
  let clos = V.create_local "clos" in
  let rec access_components i =
    if i >= arity
    then []
    else get_field_gen Asttypes.Mutable (Cvar arg) i (dbg ())
         :: access_components(i+1)
  in
  let fun_name = "caml_tuplify" ^ Int.to_string arity in
  let fun_dbg = placeholder_fun_dbg ~human_name:fun_name in
  Cfunction
   {fun_name;
    fun_args = [VP.create arg, typ_val; VP.create clos, typ_val];
    fun_body =
      Cop(Capply typ_val,
          get_field_codepointer Asttypes.Mutable (Cvar clos) 2 (dbg ())
          :: access_components 0 @ [Cvar clos],
          (dbg ()));
    fun_codegen_options = [];
    fun_poll = Default_poll;
    fun_dbg;
   }

(* Generate currying functions:
      (defun caml_curryN (arg clos)
         (alloc HDR caml_curryN_1 <arity (N-1)> caml_curry_N_1_app arg clos))
      (defun caml_curryN_1 (arg clos)
         (alloc HDR caml_curryN_2 <arity (N-2)> caml_curry_N_2_app arg clos))
      ...
      (defun caml_curryN_N-1 (arg clos)
         (let (closN-2 clos.vars[1]
               closN-3 closN-2.vars[1]
               ...
               clos1 clos2.vars[1]
               clos clos1.vars[1])
           (app clos.direct
                clos1.vars[0] ... closN-2.vars[0] clos.vars[0] arg clos)))

    Special "shortcut" functions are also generated to handle the
    case where a partially applied function is applied to all remaining
    arguments in one go.  For instance:
      (defun caml_curry_N_1_app (arg2 ... argN clos)
        (let clos' clos.vars[1]
           (app clos'.direct clos.vars[0] arg2 ... argN clos')))

    Those shortcuts may lead to a quadratic number of application
    primitives being generated in the worst case, which resulted in
    linking time blowup in practice (PR#5933), so we only generate and
    use them when below a fixed arity 'max_arity_optimized'.
*)

let max_arity_optimized = 15
let final_curry_function arity =
  let dbg = placeholder_dbg in
  let last_arg = V.create_local "arg" in
  let last_clos = V.create_local "clos" in
  let rec curry_fun args clos n =
    if n = 0 then
      Cop(Capply typ_val,
          get_field_codepointer Asttypes.Mutable (Cvar clos) 2 (dbg ()) ::
            args @ [Cvar last_arg; Cvar clos],
          dbg ())
    else
      if n = arity - 1 || arity > max_arity_optimized then
        begin
      let newclos = V.create_local "clos" in
      Clet(VP.create newclos,
           get_field_gen Asttypes.Mutable (Cvar clos) 3 (dbg ()),
           curry_fun (get_field_gen Asttypes.Mutable (Cvar clos) 2 (dbg ())
                      :: args)
             newclos (n-1))
        end else
        begin
          let newclos = V.create_local "clos" in
          Clet(VP.create newclos,
               get_field_gen Asttypes.Mutable (Cvar clos) 4 (dbg ()),
               curry_fun
                 (get_field_gen Asttypes.Mutable (Cvar clos) 3 (dbg ()) :: args)
                 newclos (n-1))
    end in
  let fun_name =
    "caml_curry" ^ Int.to_string arity ^ "_" ^ Int.to_string (arity-1)
  in
  let fun_dbg = placeholder_fun_dbg ~human_name:fun_name in
  Cfunction
   {fun_name;
    fun_args = [VP.create last_arg, typ_val; VP.create last_clos, typ_val];
    fun_body = curry_fun [] last_clos (arity-1);
    fun_codegen_options = [];
    fun_poll = Default_poll;
    fun_dbg;
   }

let rec intermediate_curry_functions arity num =
  let dbg = placeholder_dbg in
  if num = arity - 1 then
    [final_curry_function arity]
  else begin
    let name1 = "caml_curry" ^ Int.to_string arity in
    let name2 = if num = 0 then name1 else name1 ^ "_" ^ Int.to_string num in
    let arg = V.create_local "arg" and clos = V.create_local "clos" in
    let fun_dbg = placeholder_fun_dbg ~human_name:name2 in
    Cfunction
     {fun_name = name2;
      fun_args = [VP.create arg, typ_val; VP.create clos, typ_val];
      fun_body =
         if arity - num > 2 && arity <= max_arity_optimized then
           Cop(Calloc,
               [alloc_closure_header 5 (dbg ());
                Cconst_symbol(name1 ^ "_" ^ Int.to_string (num+1), dbg ());
                alloc_closure_info ~arity:(arity - num - 1)
                                   ~startenv:3 (dbg ());
                Cconst_symbol(name1 ^ "_" ^ Int.to_string (num+1) ^ "_app",
                  dbg ());
                Cvar arg; Cvar clos],
               dbg ())
         else
           Cop(Calloc,
                [alloc_closure_header 4 (dbg ());
                 Cconst_symbol(name1 ^ "_" ^ Int.to_string (num+1), dbg ());
                 alloc_closure_info ~arity:1 ~startenv:2 (dbg ());
                 Cvar arg; Cvar clos],
                dbg ());
      fun_codegen_options = [];
      fun_poll = Default_poll;
      fun_dbg;
     }
    ::
      (if arity <= max_arity_optimized && arity - num > 2 then
          let rec iter i =
            if i <= arity then
              let arg = V.create_local (Printf.sprintf "arg%d" i) in
              (arg, typ_val) :: iter (i+1)
            else []
          in
          let direct_args = iter (num+2) in
          let rec iter i args clos =
            if i = 0 then
              Cop(Capply typ_val,
                  (get_field_codepointer
                     Asttypes.Mutable (Cvar clos) 2 (dbg ()))
                  :: args @ [Cvar clos],
                  dbg ())
            else
              let newclos = V.create_local "clos" in
              Clet(VP.create newclos,
                   get_field_gen Asttypes.Mutable (Cvar clos) 4 (dbg ()),
                   iter (i-1)
                     (get_field_gen Asttypes.Mutable (Cvar clos) 3 (dbg ())
                      :: args)
                     newclos)
          in
          let fun_args =
            List.map (fun (arg, ty) -> VP.create arg, ty)
              (direct_args @ [clos, typ_val])
          in
          let fun_name = name1 ^ "_" ^ Int.to_string (num+1) ^ "_app" in
          let fun_dbg = placeholder_fun_dbg ~human_name:fun_name in
          let cf =
            Cfunction
              {fun_name;
               fun_args;
               fun_body = iter (num+1)
                  (List.map (fun (arg,_) -> Cvar arg) direct_args) clos;
               fun_codegen_options = [];
               fun_poll = Default_poll;
               fun_dbg;
              }
          in
          cf :: intermediate_curry_functions arity (num+1)
       else
          intermediate_curry_functions arity (num+1))
  end

let curry_function arity =
  assert(arity <> 0);
  (* Functions with arity = 0 does not have a curry_function *)
  if arity > 0
  then intermediate_curry_functions arity 0
  else [tuplify_function (-arity)]

module Int = Numbers.Int

let default_apply = Int.Set.add 2 (Int.Set.add 3 Int.Set.empty)
  (* These apply funs are always present in the main program because
     the run-time system needs them (cf. runtime/<arch>.S) . *)

let generic_functions shared units =
  let (apply,send,curry) =
    List.fold_left
      (fun (apply,send,curry) (ui : Cmx_format.unit_infos) ->
         List.fold_right Int.Set.add ui.ui_apply_fun apply,
         List.fold_right Int.Set.add ui.ui_send_fun send,
         List.fold_right Int.Set.add ui.ui_curry_fun curry)
      (Int.Set.empty,Int.Set.empty,Int.Set.empty)
      units in
  let apply = if shared then apply else Int.Set.union apply default_apply in
  let accu = Int.Set.fold (fun n accu -> apply_function n :: accu) apply [] in
  let accu = Int.Set.fold (fun n accu -> send_function n :: accu) send accu in
  Int.Set.fold (fun n accu -> curry_function n @ accu) curry accu

(* Primitives *)

type unary_primitive = expression -> Debuginfo.t -> expression

let floatfield n ptr dbg =
  Cop(mk_load_mut Double,
      [if n = 0 then ptr
       else Cop(Cadda, [ptr; Cconst_int(n * size_float, dbg)], dbg)],
      dbg)

let int_as_pointer arg dbg =
  Cop(Caddi, [arg; Cconst_int (-1, dbg)], dbg)
  (* always a pointer outside the heap *)

let raise_prim raise_kind arg dbg =
  if !Clflags.debug then
    Cop (Craise raise_kind, [arg], dbg)
  else
    Cop (Craise Lambda.Raise_notrace, [arg], dbg)

let negint arg dbg =
  Cop(Csubi, [Cconst_int (2, dbg); arg], dbg)

(* [offsetint] moved down to reuse add_int_caml *)

let offsetref n arg dbg =
  return_unit dbg
    (bind "ref" arg (fun arg ->
         Cop(Cstore (Word_int, Assignment),
             [arg;
              add_const (Cop(mk_load_mut Word_int, [arg], dbg))
                (n lsl 1) dbg],
             dbg)))

let arraylength kind arg dbg =
  let hdr = get_header_masked arg dbg in
  match (kind : Lambda.array_kind) with
    Pgenarray ->
      let len =
        if wordsize_shift = numfloat_shift then
          Cop(Clsr, [hdr; Cconst_int (wordsize_shift, dbg)], dbg)
        else
          bind "header" hdr (fun hdr ->
              Cifthenelse(is_addr_array_hdr hdr dbg,
                          dbg,
                          Cop(Clsr,
                            [hdr; Cconst_int (wordsize_shift, dbg)], dbg),
                          dbg,
                          Cop(Clsr,
                            [hdr; Cconst_int (numfloat_shift, dbg)], dbg),
                          dbg))
      in
      Cop(Cor, [len; Cconst_int (1, dbg)], dbg)
  | Paddrarray | Pintarray ->
      Cop(Cor, [addr_array_length_shifted hdr dbg; Cconst_int (1, dbg)], dbg)
  | Pfloatarray ->
      Cop(Cor, [float_array_length_shifted hdr dbg; Cconst_int (1, dbg)], dbg)

let bbswap bi arg dbg =
  let prim, tyarg = match (bi : Primitive.boxed_integer) with
    | Pnativeint -> "nativeint", XInt
    | Pint32 -> "int32", XInt32
    | Pint64 -> "int64", XInt64
  in
  Cop(Cextcall(Printf.sprintf "caml_%s_direct_bswap" prim,
               typ_int, [tyarg], false),
      [arg],
      dbg)

let bswap16 arg dbg =
  (Cop(Cextcall("caml_bswap16_direct", typ_int, [], false),
       [arg],
       dbg))

type binary_primitive = expression -> expression -> Debuginfo.t -> expression

(* let pfield_computed = addr_array_ref *)

(* Helper for compilation of initialization and assignment operations *)

type assignment_kind = Caml_modify | Caml_initialize | Simple

let assignment_kind
    (ptr: Lambda.immediate_or_pointer)
    (init: Lambda.initialization_or_assignment) =
  match init, ptr with
  | Assignment, Pointer -> Caml_modify
  | Heap_initialization, Pointer
  | Root_initialization, Pointer -> Caml_initialize
  | Assignment, Immediate
  | Heap_initialization, Immediate
  | Root_initialization, Immediate -> Simple

let setfield n ptr init arg1 arg2 dbg =
  match assignment_kind ptr init with
  | Caml_modify ->
      return_unit dbg
        (Cop(Cextcall("caml_modify", typ_void, [], false),
            [field_address arg1 n dbg; arg2],
            dbg))
  | Caml_initialize ->
      return_unit dbg
        (Cop(Cextcall("caml_initialize", typ_void, [], false),
             [field_address arg1 n dbg; arg2],
             dbg))
  | Simple ->
      return_unit dbg (set_field arg1 n arg2 init dbg)

let setfloatfield n init arg1 arg2 dbg =
  return_unit dbg (
    Cop(Cstore (Double, init),
        [if n = 0 then arg1
         else Cop(Cadda, [arg1; Cconst_int(n * size_float, dbg)], dbg);
         arg2], dbg))

let add_int_caml arg1 arg2 dbg =
  decr_int (add_int arg1 arg2 dbg) dbg

(* Unary primitive delayed to reuse add_int_caml *)
let offsetint n arg dbg =
  if Misc.no_overflow_lsl n 1 then
    add_const arg (n lsl 1) dbg
  else
    add_int_caml arg (int_const dbg n) dbg

let sub_int_caml arg1 arg2 dbg =
  incr_int (sub_int arg1 arg2 dbg) dbg

let mul_int_caml arg1 arg2 dbg =
  (* decrementing the non-constant part helps when the multiplication is
     followed by an addition;
     for example, using this trick compiles (100 * a + 7) into
       (+ ( * a 100) -85)
     rather than
       (+ ( * 200 (>>s a 1)) 15)
  *)
  match arg1, arg2 with
  | Cconst_int _ as c1, c2 ->
      incr_int (mul_int (untag_int c1 dbg) (decr_int c2 dbg) dbg) dbg
  | c1, c2 ->
      incr_int (mul_int (decr_int c1 dbg) (untag_int c2 dbg) dbg) dbg

let div_int_caml is_safe arg1 arg2 dbg =
  tag_int(div_int (untag_int arg1 dbg)
            (untag_int arg2 dbg) is_safe dbg) dbg

let mod_int_caml is_safe arg1 arg2 dbg =
  tag_int(mod_int (untag_int arg1 dbg)
            (untag_int arg2 dbg) is_safe dbg) dbg

let and_int_caml arg1 arg2 dbg =
  Cop(Cand, [arg1; arg2], dbg)

let or_int_caml arg1 arg2 dbg =
  Cop(Cor, [arg1; arg2], dbg)

let xor_int_caml arg1 arg2 dbg =
  Cop(Cor, [Cop(Cxor, [ignore_low_bit_int arg1;
                       ignore_low_bit_int arg2], dbg);
            Cconst_int (1, dbg)], dbg)

let lsl_int_caml arg1 arg2 dbg =
  incr_int(lsl_int (decr_int arg1 dbg)
             (untag_int arg2 dbg) dbg) dbg

let lsr_int_caml arg1 arg2 dbg =
  Cop(Cor, [lsr_int arg1 (untag_int arg2 dbg) dbg;
            Cconst_int (1, dbg)], dbg)

let asr_int_caml arg1 arg2 dbg =
  Cop(Cor, [asr_int arg1 (untag_int arg2 dbg) dbg;
            Cconst_int (1, dbg)], dbg)

let int_comp_caml cmp arg1 arg2 dbg =
  tag_int(Cop(Ccmpi cmp,
              [arg1; arg2], dbg)) dbg

let stringref_unsafe arg1 arg2 dbg =
  tag_int(Cop(mk_load_mut Byte_unsigned,
              [add_int arg1 (untag_int arg2 dbg) dbg],
              dbg)) dbg

let stringref_safe arg1 arg2 dbg =
  tag_int
    (bind "index" (untag_int arg2 dbg) (fun idx ->
      bind "str" arg1 (fun str ->
        Csequence(
          make_checkbound dbg [string_length str dbg; idx],
          Cop(mk_load_mut Byte_unsigned,
            [add_int str idx dbg], dbg))))) dbg

let string_load size unsafe arg1 arg2 dbg =
  box_sized size dbg
    (bind "index" (untag_int arg2 dbg) (fun idx ->
     bind "str" arg1 (fun str ->
       check_bound unsafe size dbg
          (string_length str dbg)
          idx (unaligned_load size str idx dbg))))

let bigstring_load size unsafe arg1 arg2 dbg =
  box_sized size dbg
   (bind "index" (untag_int arg2 dbg) (fun idx ->
    bind "ba" arg1 (fun ba ->
    bind "ba_data"
     (Cop(mk_load_mut Word_int, [field_address ba 1 dbg], dbg))
     (fun ba_data ->
        check_bound unsafe size dbg
          (bigstring_length ba dbg)
          idx
          (unaligned_load size ba_data idx dbg)))))

let arrayref_unsafe kind arg1 arg2 dbg =
  match (kind : Lambda.array_kind) with
  | Pgenarray ->
      bind "index" arg2 (fun idx ->
        bind "arr" arg1 (fun arr ->
          Cifthenelse(is_addr_array_ptr arr dbg,
                      dbg,
                      addr_array_ref arr idx dbg,
                      dbg,
                      float_array_ref arr idx dbg,
                      dbg)))
  | Paddrarray ->
      addr_array_ref arg1 arg2 dbg
  | Pintarray ->
      (* CR mshinwell: for int/addr_array_ref move "dbg" to first arg *)
      int_array_ref arg1 arg2 dbg
  | Pfloatarray ->
      float_array_ref arg1 arg2 dbg

let arrayref_safe kind arg1 arg2 dbg =
  match (kind : Lambda.array_kind) with
  | Pgenarray ->
      bind "index" arg2 (fun idx ->
      bind "arr" arg1 (fun arr ->
      bind "header" (get_header_masked arr dbg) (fun hdr ->
        if wordsize_shift = numfloat_shift then
          Csequence(
            make_checkbound dbg [addr_array_length_shifted hdr dbg; idx],
            Cifthenelse(is_addr_array_hdr hdr dbg,
                        dbg,
                        addr_array_ref arr idx dbg,
                        dbg,
                        float_array_ref arr idx dbg,
                        dbg))
        else
          Cifthenelse(is_addr_array_hdr hdr dbg,
            dbg,
            Csequence(
              make_checkbound dbg [addr_array_length_shifted hdr dbg; idx],
              addr_array_ref arr idx dbg),
            dbg,
            Csequence(
              make_checkbound dbg [float_array_length_shifted hdr dbg; idx],
              float_array_ref arr idx dbg),
            dbg))))
      | Paddrarray ->
          bind "index" arg2 (fun idx ->
          bind "arr" arg1 (fun arr ->
            Csequence(
              make_checkbound dbg [
                addr_array_length_shifted
                  (get_header_masked arr dbg) dbg; idx],
              addr_array_ref arr idx dbg)))
      | Pintarray ->
          bind "index" arg2 (fun idx ->
          bind "arr" arg1 (fun arr ->
            Csequence(
              make_checkbound dbg [
                addr_array_length_shifted
                  (get_header_masked arr dbg) dbg; idx],
              int_array_ref arr idx dbg)))
      | Pfloatarray ->
          box_float dbg (
            bind "index" arg2 (fun idx ->
            bind "arr" arg1 (fun arr ->
              Csequence(
                make_checkbound dbg [
                  float_array_length_shifted
                    (get_header_masked arr dbg) dbg;
                  idx],
                unboxed_float_array_ref arr idx dbg))))

type ternary_primitive =
  expression -> expression -> expression -> Debuginfo.t -> expression

let setfield_computed ptr init arg1 arg2 arg3 dbg =
  match assignment_kind ptr init with
  | Caml_modify ->
      return_unit dbg (addr_array_set arg1 arg2 arg3 dbg)
  | Caml_initialize ->
      return_unit dbg (addr_array_initialize arg1 arg2 arg3 dbg)
  | Simple ->
      return_unit dbg (int_array_set arg1 arg2 arg3 dbg)

let bytesset_unsafe arg1 arg2 arg3 dbg =
      return_unit dbg (Cop(Cstore (Byte_unsigned, Assignment),
                      [add_int arg1 (untag_int arg2 dbg) dbg;
                       ignore_high_bit_int (untag_int arg3 dbg)], dbg))

let bytesset_safe arg1 arg2 arg3 dbg =
  return_unit dbg
    (bind "newval" (ignore_high_bit_int (untag_int arg3 dbg)) (fun newval ->
      bind "index" (untag_int arg2 dbg) (fun idx ->
       bind "str" arg1 (fun str ->
        Csequence(
          make_checkbound dbg [string_length str dbg; idx],
          Cop(Cstore (Byte_unsigned, Assignment),
              [add_int str idx dbg; newval],
              dbg))))))

let arrayset_unsafe kind arg1 arg2 arg3 dbg =
  return_unit dbg (match (kind: Lambda.array_kind) with
  | Pgenarray ->
      bind "newval" arg3 (fun newval ->
        bind "index" arg2 (fun index ->
          bind "arr" arg1 (fun arr ->
            Cifthenelse(is_addr_array_ptr arr dbg,
                        dbg,
                        addr_array_set arr index newval dbg,
                        dbg,
                        float_array_set arr index (unbox_float dbg newval)
                          dbg,
                        dbg))))
  | Paddrarray ->
      addr_array_set arg1 arg2 arg3 dbg
  | Pintarray ->
      int_array_set arg1 arg2 arg3 dbg
  | Pfloatarray ->
      float_array_set arg1 arg2 arg3 dbg
  )

let arrayset_safe kind arg1 arg2 arg3 dbg =
  return_unit dbg (match (kind: Lambda.array_kind) with
  | Pgenarray ->
      bind "newval" arg3 (fun newval ->
      bind "index" arg2 (fun idx ->
      bind "arr" arg1 (fun arr ->
      bind "header" (get_header_masked arr dbg) (fun hdr ->
        if wordsize_shift = numfloat_shift then
          Csequence(
            make_checkbound dbg [addr_array_length_shifted hdr dbg; idx],
            Cifthenelse(is_addr_array_hdr hdr dbg,
                        dbg,
                        addr_array_set arr idx newval dbg,
                        dbg,
                        float_array_set arr idx
                          (unbox_float dbg newval)
                          dbg,
                        dbg))
        else
          Cifthenelse(
            is_addr_array_hdr hdr dbg,
            dbg,
            Csequence(
              make_checkbound dbg [addr_array_length_shifted hdr dbg; idx],
              addr_array_set arr idx newval dbg),
            dbg,
            Csequence(
              make_checkbound dbg [float_array_length_shifted hdr dbg; idx],
              float_array_set arr idx
                (unbox_float dbg newval) dbg),
            dbg)))))
  | Paddrarray ->
      bind "newval" arg3 (fun newval ->
      bind "index" arg2 (fun idx ->
      bind "arr" arg1 (fun arr ->
        Csequence(
          make_checkbound dbg [
            addr_array_length_shifted
              (get_header_masked arr dbg) dbg;
            idx],
          addr_array_set arr idx newval dbg))))
  | Pintarray ->
      bind "newval" arg3 (fun newval ->
      bind "index" arg2 (fun idx ->
      bind "arr" arg1 (fun arr ->
        Csequence(
          make_checkbound dbg [
            addr_array_length_shifted
              (get_header_masked arr dbg) dbg;
            idx],
          int_array_set arr idx newval dbg))))
  | Pfloatarray ->
      bind_load "newval" arg3 (fun newval ->
      bind "index" arg2 (fun idx ->
      bind "arr" arg1 (fun arr ->
        Csequence(
          make_checkbound dbg [
            float_array_length_shifted
              (get_header_masked arr dbg) dbg;
            idx],
          float_array_set arr idx newval dbg))))
  )

let bytes_set size unsafe arg1 arg2 arg3 dbg =
  return_unit dbg
   (bind "newval" arg3 (fun newval ->
    bind "index" (untag_int arg2 dbg) (fun idx ->
    bind "str" arg1 (fun str ->
      check_bound unsafe size dbg (string_length str dbg)
                  idx (unaligned_set size str idx newval dbg)))))

let bigstring_set size unsafe arg1 arg2 arg3 dbg =
  return_unit dbg
   (bind "newval" arg3 (fun newval ->
    bind "index" (untag_int arg2 dbg) (fun idx ->
    bind "ba" arg1 (fun ba ->
    bind "ba_data"
         (Cop(mk_load_mut Word_int, [field_address ba 1 dbg], dbg))
         (fun ba_data ->
            check_bound unsafe size dbg (bigstring_length ba dbg)
              idx (unaligned_set size ba_data idx newval dbg))))))

(* Symbols *)

let cdefine_symbol (symb, (global: Cmmgen_state.is_global)) =
  match global with
  | Global -> [Cglobal_symbol symb; Cdefine_symbol symb]
  | Local -> [Cdefine_symbol symb]

let emit_block symb white_header cont =
  (* Headers for structured constants must be marked black in case we
     are in no-naked-pointers mode.  See [caml_darken]. *)
  let black_header = Nativeint.logor white_header caml_black in
  Cint black_header :: cdefine_symbol symb @ cont

let emit_string_constant_fields s cont =
  let n = size_int - 1 - (String.length s) mod size_int in
  Cstring s :: Cskip n :: Cint8 n :: cont

let emit_boxed_int32_constant_fields n cont =
  let n = Nativeint.of_int32 n in
  Csymbol_address caml_int32_ops :: Cint32 n :: Cint32 0n :: cont

let emit_boxed_int64_constant_fields n cont =
  let lo = Int64.to_nativeint n in
  Csymbol_address caml_int64_ops :: Cint lo :: cont

let emit_boxed_nativeint_constant_fields n cont =
  Csymbol_address caml_nativeint_ops :: Cint n :: cont

let emit_float_constant symb f cont =
  emit_block symb float_header (Cdouble f :: cont)

let emit_string_constant symb s cont =
  emit_block symb (string_header (String.length s))
    (emit_string_constant_fields s cont)

let emit_int32_constant symb n cont =
  emit_block symb boxedint32_header
    (emit_boxed_int32_constant_fields n cont)

let emit_int64_constant symb n cont =
  emit_block symb boxedint64_header
    (emit_boxed_int64_constant_fields n cont)

let emit_nativeint_constant symb n cont =
  emit_block symb boxedintnat_header
    (emit_boxed_nativeint_constant_fields n cont)

let emit_float_array_constant symb fields cont =
  emit_block symb (floatarray_header (List.length fields))
    (Misc.map_end (fun f -> Cdouble f) fields cont)

(* Generate the entry point *)

let entry_point namelist =
  let dbg = placeholder_dbg in
  let cconst_int i = Cconst_int (i, dbg ()) in
  let cconst_symbol sym = Cconst_symbol (sym, dbg ()) in
  let incr_global_inited () =
    Cop(Cstore (Word_int, Assignment),
        [cconst_symbol "caml_globals_inited";
         Cop(Caddi, [Cop(mk_load_mut Word_int,
                       [cconst_symbol "caml_globals_inited"], dbg ());
                     cconst_int 1], dbg ())], dbg ()) in
  let body =
    List.fold_right
      (fun name next ->
        let entry_sym = Compilenv.make_symbol ~unitname:name (Some "entry") in
        Csequence(Cop(Capply typ_void,
                         [cconst_symbol entry_sym], dbg ()),
                  Csequence(incr_global_inited (), next)))
      namelist (cconst_int 1) in
  let fun_name = "caml_program" in
  let fun_dbg = placeholder_fun_dbg ~human_name:fun_name in
  Cfunction {fun_name;
             fun_args = [];
             fun_body = body;
             fun_codegen_options = [Reduce_code_size];
             fun_poll = Default_poll;
             fun_dbg;
            }

(* Generate the table of globals *)

let cint_zero = Cint 0n

let global_table namelist =
  let mksym name =
    Csymbol_address (Compilenv.make_symbol ~unitname:name (Some "gc_roots"))
  in
  Cdata(Cglobal_symbol "caml_globals" ::
        Cdefine_symbol "caml_globals" ::
        List.map mksym namelist @
        [cint_zero])

let reference_symbols namelist =
  let mksym name = Csymbol_address name in
  Cdata(List.map mksym namelist)

let global_data name v =
  Cdata(emit_string_constant (name, Global)
          (Marshal.to_string v []) [])

let globals_map v = global_data "caml_globals_map" v

(* Generate the master table of frame descriptors *)

let frame_table namelist =
  let mksym name =
    Csymbol_address (Compilenv.make_symbol ~unitname:name (Some "frametable"))
  in
  Cdata(Cglobal_symbol "caml_frametable" ::
        Cdefine_symbol "caml_frametable" ::
        List.map mksym namelist
        @ [cint_zero])

(* Generate the table of module data and code segments *)

let segment_table namelist symbol begname endname =
  let addsyms name lst =
    Csymbol_address (Compilenv.make_symbol ~unitname:name (Some begname)) ::
    Csymbol_address (Compilenv.make_symbol ~unitname:name (Some endname)) ::
    lst
  in
  Cdata(Cglobal_symbol symbol ::
        Cdefine_symbol symbol ::
        List.fold_right addsyms namelist [cint_zero])

let data_segment_table namelist =
  segment_table namelist "caml_data_segments" "data_begin" "data_end"

let code_segment_table namelist =
  segment_table namelist "caml_code_segments" "code_begin" "code_end"

(* Initialize a predefined exception *)

let predef_exception i name =
  let name_sym = Compilenv.new_const_symbol () in
  let data_items =
    emit_string_constant (name_sym, Local) name []
  in
  let exn_sym = "caml_exn_" ^ name in
  let tag = Obj.object_tag in
  let size = 2 in
  let fields =
    (Csymbol_address name_sym)
      :: (cint_const (-i - 1))
      :: data_items
  in
  let data_items =
    emit_block (exn_sym, Global) (block_header tag size) fields
  in
  Cdata data_items

(* Header for a plugin *)

let plugin_header units =
  let mk ((ui : Cmx_format.unit_infos),crc) : Cmxs_format.dynunit =
    { dynu_name = ui.ui_name;
      dynu_crc = crc;
      dynu_imports_cmi = ui.ui_imports_cmi;
      dynu_imports_cmx = ui.ui_imports_cmx;
      dynu_defines = ui.ui_defines
    } in
  global_data "caml_plugin_header"
    ({ dynu_magic = Config.cmxs_magic_number;
       dynu_units = List.map mk units }
     : Cmxs_format.dynheader)

(* To compile "let rec" over values *)

let fundecls_size fundecls =
  let sz = ref (-1) in
  List.iter
    (fun (f : Clambda.ufunction) ->
       let indirect_call_code_pointer_size =
         match f.arity with
         | 0 | 1 -> 0
           (* arity 1 does not need an indirect call handler.
              arity 0 cannot be indirect called *)
         | _ -> 1
           (* For other arities there is an indirect call handler.
              if arity >= 2 it is caml_curry...
              if arity < 0 it is caml_tuplify... *)
       in
       sz := !sz + 1 + 2 + indirect_call_code_pointer_size)
    fundecls;
  !sz

(* Emit constant closures *)

let emit_constant_closure ((_, global_symb) as symb) fundecls clos_vars cont =
  let closure_symbol (f : Clambda.ufunction) =
    if Config.flambda then
      cdefine_symbol (f.label ^ "_closure", global_symb)
    else
      []
  in
  match (fundecls : Clambda.ufunction list) with
    [] ->
      (* This should probably not happen: dead code has normally been
         eliminated and a closure cannot be accessed without going through
         a [Project_closure], which depends on the function. *)
      assert (clos_vars = []);
      cdefine_symbol symb @ clos_vars @ cont
  | f1 :: remainder ->
      let startenv = fundecls_size fundecls in
      let rec emit_others pos = function
          [] -> clos_vars @ cont
      | (f2 : Clambda.ufunction) :: rem ->
          if f2.arity = 1 || f2.arity = 0 then
            Cint(infix_header pos) ::
            (closure_symbol f2) @
            Csymbol_address f2.label ::
            Cint(closure_info ~arity:f2.arity ~startenv:(startenv - pos)) ::
            emit_others (pos + 3) rem
          else
            Cint(infix_header pos) ::
            (closure_symbol f2) @
            Csymbol_address(curry_function_sym f2.arity) ::
            Cint(closure_info ~arity:f2.arity ~startenv:(startenv - pos)) ::
            Csymbol_address f2.label ::
            emit_others (pos + 4) rem in
      Cint(black_closure_header (fundecls_size fundecls
                                 + List.length clos_vars)) ::
      cdefine_symbol symb @
      (closure_symbol f1) @
      if f1.arity = 1 || f1.arity = 0 then
        Csymbol_address f1.label ::
        Cint(closure_info ~arity:f1.arity ~startenv) ::
        emit_others 3 remainder
      else
        Csymbol_address(curry_function_sym f1.arity) ::
        Cint(closure_info ~arity:f1.arity ~startenv) ::
        Csymbol_address f1.label ::
        emit_others 4 remainder

(* Build the NULL terminated array of gc roots *)

let emit_gc_roots_table ~symbols cont =
  let table_symbol = Compilenv.make_symbol (Some "gc_roots") in
  Cdata(Cglobal_symbol table_symbol ::
        Cdefine_symbol table_symbol ::
        List.map (fun s -> Csymbol_address s) symbols @
        [Cint 0n])
  :: cont

(* Build preallocated blocks (used for Flambda [Initialize_symbol]
   constructs, and Clambda global module) *)

let preallocate_block cont { Clambda.symbol; exported; tag; fields } =
  let space =
    (* These words will be registered as roots and as such must contain
       valid values, in case we are in no-naked-pointers mode.  Likewise
       the block header must be black, below (see [caml_darken]), since
       the overall record may be referenced. *)
    List.map (fun field ->
        match field with
        | None ->
            Cint (Nativeint.of_int 1 (* Val_unit *))
        | Some (Clambda.Uconst_field_int n) ->
            cint_const n
        | Some (Clambda.Uconst_field_ref label) ->
            Csymbol_address label)
      fields
  in
  let global = Cmmgen_state.(if exported then Global else Local) in
  let symb = (symbol, global) in
  let data =
    emit_block symb (block_header tag (List.length fields)) space
  in
  Cdata data :: cont

let emit_preallocated_blocks preallocated_blocks cont =
  let symbols =
    List.map (fun ({ Clambda.symbol }:Clambda.preallocated_block) -> symbol)
      preallocated_blocks
  in
  let c1 = emit_gc_roots_table ~symbols cont in
  List.fold_left preallocate_block c1 preallocated_blocks
