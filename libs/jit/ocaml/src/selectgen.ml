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

(* Selection of pseudo-instructions, assignment of pseudo-registers,
   sequentialization. *)

open Cmm
open Reg
open Mach

module Int = Numbers.Int
module V = Backend_var
module VP = Backend_var.With_provenance

type environment =
  { vars : (Reg.t array
            * Backend_var.Provenance.t option
            * Asttypes.mutable_flag) V.Map.t;
    static_exceptions : Reg.t array list Int.Map.t;
    (** Which registers must be populated when jumping to the given
        handler. *)
  }

let env_add ?(mut=Asttypes.Immutable) var regs env =
  let provenance = VP.provenance var in
  let var = VP.var var in
  { env with vars = V.Map.add var (regs, provenance, mut) env.vars }

let env_add_static_exception id v env =
  { env with static_exceptions = Int.Map.add id v env.static_exceptions }

let env_find id env =
  let regs, _provenance, _mut = V.Map.find id env.vars in
  regs

let env_find_mut id env =
  let regs, _provenance, mut = V.Map.find id env.vars in
  begin match mut with
  | Asttypes.Mutable -> ()
  | Asttypes.Immutable ->
    Misc.fatal_error "Selectgen.env_find_mut: not mutable"
  end;
  regs

let env_find_static_exception id env =
  Int.Map.find id env.static_exceptions

let env_empty = {
  vars = V.Map.empty;
  static_exceptions = Int.Map.empty;
}

(* Infer the type of the result of an operation *)

let oper_result_type = function
    Capply ty -> ty
  | Cextcall(_s, ty_res, _ty_args, _alloc) -> ty_res
  | Cload {memory_chunk} ->
      begin match memory_chunk with
      | Word_val -> typ_val
      | Single | Double -> typ_float
      | _ -> typ_int
      end
  | Calloc -> typ_val
  | Cstore (_c, _) -> typ_void
  | Cdls_get -> typ_val
  | Caddi | Csubi | Cmuli | Cmulhi | Cdivi | Cmodi |
    Cand | Cor | Cxor | Clsl | Clsr | Casr |
    Ccmpi _ | Ccmpa _ | Ccmpf _ -> typ_int
  | Caddv -> typ_val
  | Cadda -> typ_addr
  | Cnegf | Cabsf | Caddf | Csubf | Cmulf | Cdivf -> typ_float
  | Cfloatofint -> typ_float
  | Cintoffloat -> typ_int
  | Craise _ -> typ_void
  | Ccheckbound -> typ_void
  | Copaque -> typ_val
  | Cpoll -> typ_void

(* Infer the size in bytes of the result of an expression whose evaluation
   may be deferred (cf. [emit_parts]). *)

let size_component = function
  | Val | Addr -> Arch.size_addr
  | Int -> Arch.size_int
  | Float -> Arch.size_float

let size_machtype mty =
  let size = ref 0 in
  for i = 0 to Array.length mty - 1 do
    size := !size + size_component mty.(i)
  done;
  !size

let size_expr (env:environment) exp =
  let rec size localenv = function
      Cconst_int _ | Cconst_natint _ -> Arch.size_int
    | Cconst_symbol _ ->
        Arch.size_addr
    | Cconst_float _ -> Arch.size_float
    | Cvar id ->
        begin try
          V.Map.find id localenv
        with Not_found ->
        try
          let regs = env_find id env in
          size_machtype (Array.map (fun r -> r.typ) regs)
        with Not_found ->
          Misc.fatal_error("Selection.size_expr: unbound var " ^
                           V.unique_name id)
        end
    | Ctuple el ->
        List.fold_right (fun e sz -> size localenv e + sz) el 0
    | Cop(op, _, _) ->
        size_machtype(oper_result_type op)
    | Clet(id, arg, body) ->
        size (V.Map.add (VP.var id) (size localenv arg) localenv) body
    | Csequence(_e1, e2) ->
        size localenv e2
    | _ ->
        Misc.fatal_error "Selection.size_expr"
  in size V.Map.empty exp

(* Swap the two arguments of an integer comparison *)

let swap_intcomp = function
    Isigned cmp -> Isigned(swap_integer_comparison cmp)
  | Iunsigned cmp -> Iunsigned(swap_integer_comparison cmp)

(* Naming of registers *)

let all_regs_anonymous rv =
  try
    for i = 0 to Array.length rv - 1 do
      if not (Reg.anonymous rv.(i)) then raise Exit
    done;
    true
  with Exit ->
    false

let name_regs id rv =
  let id = VP.var id in
  if Array.length rv = 1 then
    rv.(0).raw_name <- Raw_name.create_from_var id
  else
    for i = 0 to Array.length rv - 1 do
      rv.(i).raw_name <- Raw_name.create_from_var id;
      rv.(i).part <- Some i
    done

(* "Join" two instruction sequences, making sure they return their results
   in the same registers. *)

let join env opt_r1 seq1 opt_r2 seq2 =
  match (opt_r1, opt_r2) with
    (None, _) -> opt_r2
  | (_, None) -> opt_r1
  | (Some r1, Some r2) ->
      let l1 = Array.length r1 in
      assert (l1 = Array.length r2);
      let r = Array.make l1 Reg.dummy in
      for i = 0 to l1-1 do
        if Reg.anonymous r1.(i)
          && Cmm.ge_component r1.(i).typ r2.(i).typ
        then begin
          r.(i) <- r1.(i);
          seq2#insert_move env r2.(i) r1.(i)
        end else if Reg.anonymous r2.(i)
          && Cmm.ge_component r2.(i).typ r1.(i).typ
        then begin
          r.(i) <- r2.(i);
          seq1#insert_move env r1.(i) r2.(i)
        end else begin
          let typ = Cmm.lub_component r1.(i).typ r2.(i).typ in
          r.(i) <- Reg.create typ;
          seq1#insert_move env r1.(i) r.(i);
          seq2#insert_move env r2.(i) r.(i)
        end
      done;
      Some r

(* Same, for N branches *)

let join_array env rs =
  let some_res = ref None in
  for i = 0 to Array.length rs - 1 do
    let (r, _) = rs.(i) in
    match r with
    | None -> ()
    | Some r ->
      match !some_res with
      | None -> some_res := Some (r, Array.map (fun r -> r.typ) r)
      | Some (r', types) ->
        let types =
          Array.map2 (fun r typ -> Cmm.lub_component r.typ typ) r types
        in
        some_res := Some (r', types)
  done;
  match !some_res with
    None -> None
  | Some (template, types) ->
      let size_res = Array.length template in
      let res = Array.make size_res Reg.dummy in
      for i = 0 to size_res - 1 do
        res.(i) <- Reg.create types.(i)
      done;
      for i = 0 to Array.length rs - 1 do
        let (r, s) = rs.(i) in
        match r with
          None -> ()
        | Some r -> s#insert_moves env r res
      done;
      Some res

(* Name of function being compiled *)
let current_function_name = ref ""

module Effect = struct
  type t =
    | None
    | Raise
    | Arbitrary

  let join t1 t2 =
    match t1, t2 with
    | None, t2 -> t2
    | t1, None -> t1
    | Raise, Raise -> Raise
    | Arbitrary, _ | _, Arbitrary -> Arbitrary

  let pure = function
    | None -> true
    | Raise | Arbitrary -> false
end

module Coeffect = struct
  type t =
    | None
    | Read_mutable
    | Arbitrary

  let join t1 t2 =
    match t1, t2 with
    | None, t2 -> t2
    | t1, None -> t1
    | Read_mutable, Read_mutable -> Read_mutable
    | Arbitrary, _ | _, Arbitrary -> Arbitrary

  let copure = function
    | None -> true
    | Read_mutable | Arbitrary -> false
end

module Effect_and_coeffect : sig
  type t

  val none : t
  val arbitrary : t

  val effect_ : t -> Effect.t
  val coeffect : t -> Coeffect.t

  val pure_and_copure : t -> bool

  val effect_only : Effect.t -> t
  val coeffect_only : Coeffect.t -> t

  val join : t -> t -> t
  val join_list_map : 'a list -> ('a -> t) -> t
end = struct
  type t = Effect.t * Coeffect.t

  let none = Effect.None, Coeffect.None
  let arbitrary = Effect.Arbitrary, Coeffect.Arbitrary

  let effect_ (e, _ce) = e
  let coeffect (_e, ce) = ce

  let pure_and_copure (e, ce) = Effect.pure e && Coeffect.copure ce

  let effect_only e = e, Coeffect.None
  let coeffect_only ce = Effect.None, ce

  let join (e1, ce1) (e2, ce2) =
    Effect.join e1 e2, Coeffect.join ce1 ce2

  let join_list_map xs f =
    match xs with
    | [] -> none
    | x::xs -> List.fold_left (fun acc x -> join acc (f x)) (f x) xs
end

(* The default instruction selection class *)

class virtual selector_generic = object (self)

(* A syntactic criterion used in addition to judgements about (co)effects as
   to whether the evaluation of a given expression may be deferred by
   [emit_parts].  This criterion is a property of the instruction selection
   algorithm in this file rather than a property of the Cmm language.
*)
method is_simple_expr = function
    Cconst_int _ -> true
  | Cconst_natint _ -> true
  | Cconst_float _ -> true
  | Cconst_symbol _ -> true
  | Cvar _ -> true
  | Creturn_addr -> true
  | Ctuple el -> List.for_all self#is_simple_expr el
  | Clet(_id, arg, body) | Clet_mut(_id, _, arg, body) ->
    self#is_simple_expr arg && self#is_simple_expr body
  | Cphantom_let(_var, _defining_expr, body) -> self#is_simple_expr body
  | Csequence(e1, e2) -> self#is_simple_expr e1 && self#is_simple_expr e2
  | Cop(op, args, _) ->
      begin match op with
        (* The following may have side effects *)
      | Capply _ | Cextcall _ | Calloc | Cstore _ | Craise _ | Copaque
      | Cpoll -> false
        (* The remaining operations are simple if their args are *)
      | Cload _ | Caddi | Csubi | Cmuli | Cmulhi | Cdivi | Cmodi | Cand | Cor
      | Cxor | Clsl | Clsr | Casr | Ccmpi _ | Caddv | Cadda | Ccmpa _ | Cnegf
      | Cabsf | Caddf | Csubf | Cmulf | Cdivf | Cfloatofint | Cintoffloat
      | Ccmpf _ | Ccheckbound | Cdls_get ->
          List.for_all self#is_simple_expr args
      end
  | Cassign _ | Cifthenelse _ | Cswitch _ | Ccatch _ | Cexit _
  | Ctrywith _ -> false

(* Analyses the effects and coeffects of an expression.  This is used across
   a whole list of expressions with a view to determining which expressions
   may have their evaluation deferred.  The result of this function, modulo
   target-specific judgements if the [effects_of] method is overridden, is a
   property of the Cmm language rather than anything particular about the
   instruction selection algorithm in this file.

   In the case of e.g. an OCaml function call, the arguments whose evaluation
   cannot be deferred (cf. [emit_parts], below) are computed in right-to-left
   order first with their results going into temporaries, then the block is
   allocated, then the remaining arguments are evaluated before being
   combined with the temporaries. *)
method effects_of exp =
  let module EC = Effect_and_coeffect in
  match exp with
  | Cconst_int _ | Cconst_natint _ | Cconst_float _ | Cconst_symbol _
  | Cvar _ | Creturn_addr -> EC.none
  | Ctuple el -> EC.join_list_map el self#effects_of
  | Clet (_id, arg, body) | Clet_mut (_id, _, arg, body) ->
    EC.join (self#effects_of arg) (self#effects_of body)
  | Cphantom_let (_var, _defining_expr, body) -> self#effects_of body
  | Csequence (e1, e2) ->
    EC.join (self#effects_of e1) (self#effects_of e2)
  | Cifthenelse (cond, _ifso_dbg, ifso, _ifnot_dbg, ifnot, _dbg) ->
    EC.join (self#effects_of cond)
      (EC.join (self#effects_of ifso) (self#effects_of ifnot))
  | Cop (op, args, _) ->
    let from_op =
      match op with
      | Capply _ | Cextcall _ | Copaque | Cpoll -> EC.arbitrary
      | Calloc -> EC.none
      | Cstore _ -> EC.effect_only Effect.Arbitrary
      | Craise _ | Ccheckbound -> EC.effect_only Effect.Raise
      | Cload {mutability = Asttypes.Immutable} -> EC.none
      | Cload {mutability = Asttypes.Mutable} | Cdls_get ->
          EC.coeffect_only Coeffect.Read_mutable
      | Caddi | Csubi | Cmuli | Cmulhi | Cdivi | Cmodi | Cand | Cor | Cxor
      | Clsl | Clsr | Casr | Ccmpi _ | Caddv | Cadda | Ccmpa _ | Cnegf | Cabsf
      | Caddf | Csubf | Cmulf | Cdivf | Cfloatofint | Cintoffloat | Ccmpf _ ->
        EC.none
    in
    EC.join from_op (EC.join_list_map args self#effects_of)
  | Cassign _ | Cswitch _ | Ccatch _ | Cexit _ | Ctrywith _ ->
    EC.arbitrary

(* Says whether an integer constant is a suitable immediate argument for
   the given integer operation *)

method is_immediate op n =
  match op with
  | Ilsl | Ilsr | Iasr -> n >= 0 && n < Arch.size_int * 8
  | _ -> false

(* Says whether an integer constant is a suitable immediate argument for
   the given integer test *)

method virtual is_immediate_test : integer_comparison -> int -> bool

(* Selection of addressing modes *)

method virtual select_addressing :
  Cmm.memory_chunk -> Cmm.expression -> Arch.addressing_mode * Cmm.expression

(* Default instruction selection for stores (of words) *)

method select_store is_assign addr arg =
  (Istore(Word_val, addr, is_assign), arg)

(* Default instruction selection for operators *)

method select_operation op args _dbg =
  match (op, args) with
  | (Capply _, Cconst_symbol (func, _dbg) :: rem) ->
    (Icall_imm { func; }, rem)
  | (Capply _, _) ->
    (Icall_ind, args)
  | (Cextcall(func, ty_res, ty_args, alloc), _) ->
    Iextcall { func; alloc; ty_res; ty_args; stack_ofs = -1}, args
  | (Cload {memory_chunk; mutability; is_atomic}, [arg]) ->
      let (addressing_mode, eloc) = self#select_addressing memory_chunk arg in
      (Iload {memory_chunk; addressing_mode; mutability; is_atomic}, [eloc])
  | (Cstore (chunk, init), [arg1; arg2]) ->
      let (addr, eloc) = self#select_addressing chunk arg1 in
      let is_assign =
        match init with
        | Lambda.Root_initialization -> false
        | Lambda.Heap_initialization -> false
        | Lambda.Assignment -> true
      in
      if chunk = Word_int || chunk = Word_val then begin
        let (op, newarg2) = self#select_store is_assign addr arg2 in
        (op, [newarg2; eloc])
      end else begin
        (Istore(chunk, addr, is_assign), [arg2; eloc])
        (* Inversion addr/datum in Istore *)
      end
  | (Cdls_get, _) -> Idls_get, args
  | (Cpoll, _) -> (Ipoll { return_label = None }), args
  | (Calloc, _) -> (Ialloc {bytes = 0; dbginfo = []}), args
  | (Caddi, _) -> self#select_arith_comm Iadd args
  | (Csubi, _) -> self#select_arith Isub args
  | (Cmuli, _) -> self#select_arith_comm Imul args
  | (Cmulhi, _) -> self#select_arith_comm Imulh args
  | (Cdivi, _) -> (Iintop Idiv, args)
  | (Cmodi, _) -> (Iintop Imod, args)
  | (Cand, _) -> self#select_arith_comm Iand args
  | (Cor, _) -> self#select_arith_comm Ior args
  | (Cxor, _) -> self#select_arith_comm Ixor args
  | (Clsl, _) -> self#select_arith Ilsl args
  | (Clsr, _) -> self#select_arith Ilsr args
  | (Casr, _) -> self#select_arith Iasr args
  | (Ccmpi comp, _) -> self#select_arith_comp (Isigned comp) args
  | (Caddv, _) -> self#select_arith_comm Iadd args
  | (Cadda, _) -> self#select_arith_comm Iadd args
  | (Ccmpa comp, _) -> self#select_arith_comp (Iunsigned comp) args
  | (Ccmpf comp, _) -> (Icompf comp, args)
  | (Cnegf, _) -> (Inegf, args)
  | (Cabsf, _) -> (Iabsf, args)
  | (Caddf, _) -> (Iaddf, args)
  | (Csubf, _) -> (Isubf, args)
  | (Cmulf, _) -> (Imulf, args)
  | (Cdivf, _) -> (Idivf, args)
  | (Cfloatofint, _) -> (Ifloatofint, args)
  | (Cintoffloat, _) -> (Iintoffloat, args)
  | (Ccheckbound, _) ->
    self#select_arith Icheckbound args
  | _ -> Misc.fatal_error "Selection.select_oper"

method private select_arith_comm op = function
  | [arg; Cconst_int (n, _)] when self#is_immediate op n ->
      (Iintop_imm(op, n), [arg])
  | [Cconst_int (n, _); arg] when self#is_immediate op n ->
      (Iintop_imm(op, n), [arg])
  | args ->
      (Iintop op, args)

method private select_arith op = function
  | [arg; Cconst_int (n, _)] when self#is_immediate op n ->
      (Iintop_imm(op, n), [arg])
  | args ->
      (Iintop op, args)

method private select_arith_comp cmp = function
  | [arg; Cconst_int (n, _)] when self#is_immediate (Icomp cmp) n ->
      (Iintop_imm(Icomp cmp, n), [arg])
  | [Cconst_int (n, _); arg]
    when self#is_immediate (Icomp(swap_intcomp cmp)) n ->
      (Iintop_imm(Icomp(swap_intcomp cmp), n), [arg])
  | args ->
      (Iintop(Icomp cmp), args)

(* Instruction selection for conditionals *)

method select_condition = function
  | Cop(Ccmpi cmp, [arg1; Cconst_int (n, _)], _)
    when self#is_immediate_test (Isigned cmp) n ->
      (Iinttest_imm(Isigned cmp, n), arg1)
  | Cop(Ccmpi cmp, [Cconst_int (n, _); arg2], _)
    when self#is_immediate_test (Isigned (swap_integer_comparison cmp)) n ->
      (Iinttest_imm(Isigned(swap_integer_comparison cmp), n), arg2)
  | Cop(Ccmpi cmp, args, _) ->
      (Iinttest(Isigned cmp), Ctuple args)
  | Cop(Ccmpa cmp, [arg1; Cconst_int (n, _)], _)
    when self#is_immediate_test (Iunsigned cmp) n ->
      (Iinttest_imm(Iunsigned cmp, n), arg1)
  | Cop(Ccmpa cmp, [Cconst_int (n, _); arg2], _)
    when self#is_immediate_test (Iunsigned (swap_integer_comparison cmp)) n ->
      (Iinttest_imm(Iunsigned(swap_integer_comparison cmp), n), arg2)
  | Cop(Ccmpa cmp, args, _) ->
      (Iinttest(Iunsigned cmp), Ctuple args)
  | Cop(Ccmpf cmp, args, _) ->
      (Ifloattest cmp, Ctuple args)
  | Cop(Cand, [arg; Cconst_int (1, _)], _) ->
      (Ioddtest, arg)
  | arg ->
      (Itruetest, arg)

(* Return an array of fresh registers of the given type.
   Normally implemented as Reg.createv, but some
   ports (e.g. Arm) can override this definition to store float values
   in pairs of integer registers. *)

method regs_for tys = Reg.createv tys

(* Buffering of instruction sequences *)

val mutable instr_seq = dummy_instr

method insert_debug _env desc dbg arg res =
  instr_seq <- instr_cons_debug desc arg res dbg instr_seq

method insert _env desc arg res =
  instr_seq <- instr_cons desc arg res instr_seq

method extract_onto o =
  let rec extract res i =
    if i == dummy_instr
      then res
      else extract {i with next = res} i.next in
    extract o instr_seq

method extract =
  self#extract_onto (end_instr ())

(* Insert a sequence of moves from one pseudoreg set to another. *)

method insert_move env src dst =
  if src.stamp <> dst.stamp then
    self#insert env (Iop Imove) [|src|] [|dst|]

method insert_moves env src dst =
  for i = 0 to Stdlib.Int.min (Array.length src) (Array.length dst) - 1 do
    self#insert_move env src.(i) dst.(i)
  done

(* Insert moves and stack offsets for function arguments and results *)

method insert_move_args env arg loc stacksize =
  if stacksize <> 0 then begin
    self#insert env (Iop(Istackoffset stacksize)) [||] [||]
  end;
  self#insert_moves env arg loc

method insert_move_results env loc res stacksize =
  if stacksize <> 0 then begin
    self#insert env (Iop(Istackoffset(-stacksize))) [||] [||]
  end;
  self#insert_moves env loc res

(* Add an Iop opcode. Can be overridden by processor description
   to insert moves before and after the operation, i.e. for two-address
   instructions, or instructions using dedicated registers. *)

method insert_op_debug env op dbg rs rd =
  self#insert_debug env (Iop op) dbg rs rd;
  rd

method insert_op env op rs rd =
  self#insert_op_debug env op Debuginfo.none rs rd

(* Add the instructions for the given expression
   at the end of the self sequence *)

method emit_expr (env:environment) exp =
  match exp with
    Cconst_int (n, _dbg) ->
      let r = self#regs_for typ_int in
      Some(self#insert_op env (Iconst_int(Nativeint.of_int n)) [||] r)
  | Cconst_natint (n, _dbg) ->
      let r = self#regs_for typ_int in
      Some(self#insert_op env (Iconst_int n) [||] r)
  | Cconst_float (n, _dbg) ->
      let r = self#regs_for typ_float in
      Some(self#insert_op env (Iconst_float (Int64.bits_of_float n)) [||] r)
  | Cconst_symbol (n, _dbg) ->
      (* Cconst_symbol _ evaluates to a statically-allocated address, so its
         value fits in a typ_int register and is never changed by the GC.

         Some Cconst_symbols point to statically-allocated blocks, some of
         which may point to heap values. However, any such blocks will be
         registered in the compilation unit's global roots structure, so
         adding this register to the frame table would be redundant *)
      let r = self#regs_for typ_int in
      Some(self#insert_op env (Iconst_symbol n) [||] r)
  | Creturn_addr ->
      let r = self#regs_for typ_int in
      Some(self#insert_op env Ireturn_addr [||] r)
  | Cvar v ->
      begin try
        Some(env_find v env)
      with Not_found ->
        Misc.fatal_error("Selection.emit_expr: unbound var " ^ V.unique_name v)
      end
  | Clet(v, e1, e2) ->
      begin match self#emit_expr env e1 with
        None -> None
      | Some r1 -> self#emit_expr (self#bind_let env v r1) e2
      end
  | Clet_mut(v, k, e1, e2) ->
      begin match self#emit_expr env e1 with
        None -> None
      | Some r1 -> self#emit_expr (self#bind_let_mut env v k r1) e2
      end
  | Cphantom_let (_var, _defining_expr, body) ->
      self#emit_expr env body
  | Cassign(v, e1) ->
      let rv =
        try
          env_find_mut v env
        with Not_found ->
          Misc.fatal_error ("Selection.emit_expr: unbound var " ^ V.name v) in
      begin match self#emit_expr env e1 with
        None -> None
      | Some r1 ->
          self#insert_moves env r1 rv; Some [||]
      end
  | Ctuple [] ->
      Some [||]
  | Ctuple exp_list ->
      begin match self#emit_parts_list env exp_list with
        None -> None
      | Some(simple_list, ext_env) ->
          Some(self#emit_tuple ext_env simple_list)
      end
  | Cop(Craise k, [arg], dbg) ->
      begin match self#emit_expr env arg with
        None -> None
      | Some r1 ->
          let rd = [|Proc.loc_exn_bucket|] in
          self#insert env (Iop Imove) r1 rd;
          self#insert_debug env  (Iraise k) dbg rd [||];
          None
      end
  | Cop(Copaque, args, dbg) ->
      begin match self#emit_parts_list env args with
        None -> None
      | Some (simple_args, env) ->
         let rs = self#emit_tuple env simple_args in
         Some (self#insert_op_debug env Iopaque dbg rs rs)
      end
  | Cop(op, args, dbg) ->
      begin match self#emit_parts_list env args with
        None -> None
      | Some(simple_args, env) ->
          let ty = oper_result_type op in
          let (new_op, new_args) = self#select_operation op simple_args dbg in
          match new_op with
            Icall_ind ->
              let r1 = self#emit_tuple env new_args in
              let rarg = Array.sub r1 1 (Array.length r1 - 1) in
              let rd = self#regs_for ty in
              let (loc_arg, stack_ofs) = Proc.loc_arguments (Reg.typv rarg) in
              let loc_res = Proc.loc_results (Reg.typv rd) in
              self#insert_move_args env rarg loc_arg stack_ofs;
              self#insert_debug env (Iop new_op) dbg
                          (Array.append [|r1.(0)|] loc_arg) loc_res;
              self#insert_move_results env loc_res rd stack_ofs;
              Some rd
          | Icall_imm _ ->
              let r1 = self#emit_tuple env new_args in
              let rd = self#regs_for ty in
              let (loc_arg, stack_ofs) = Proc.loc_arguments (Reg.typv r1) in
              let loc_res = Proc.loc_results (Reg.typv rd) in
              self#insert_move_args env r1 loc_arg stack_ofs;
              self#insert_debug env (Iop new_op) dbg loc_arg loc_res;
              self#insert_move_results env loc_res rd stack_ofs;
              Some rd
          | Iextcall r ->
              let (loc_arg, stack_ofs) =
                self#emit_extcall_args env r.ty_args new_args in
              let rd = self#regs_for ty in
              let loc_res =
                self#insert_op_debug env
                  (Iextcall {r with stack_ofs = stack_ofs}) dbg
                  loc_arg (Proc.loc_external_results (Reg.typv rd)) in
              self#insert_move_results env loc_res rd stack_ofs;
              Some rd
          | Ialloc { bytes = _; } ->
              let rd = self#regs_for typ_val in
              let bytes = size_expr env (Ctuple new_args) in
              assert (bytes mod Arch.size_addr = 0);
              let alloc_words = bytes / Arch.size_addr in
              let op =
                Ialloc { bytes; dbginfo = [{alloc_words; alloc_dbg = dbg}] }
              in
              self#insert_debug env (Iop op) dbg [||] rd;
              self#emit_stores env new_args rd;
              Some rd
          | op ->
              let r1 = self#emit_tuple env new_args in
              let rd = self#regs_for ty in
              Some (self#insert_op_debug env op dbg r1 rd)
      end
  | Csequence(e1, e2) ->
      begin match self#emit_expr env e1 with
        None -> None
      | Some _ -> self#emit_expr env e2
      end
  | Cifthenelse(econd, _ifso_dbg, eif, _ifnot_dbg, eelse, _dbg) ->
      let (cond, earg) = self#select_condition econd in
      begin match self#emit_expr env earg with
        None -> None
      | Some rarg ->
          let (rif, sif) = self#emit_sequence env eif in
          let (relse, selse) = self#emit_sequence env eelse in
          let r = join env rif sif relse selse in
          self#insert env (Iifthenelse(cond, sif#extract, selse#extract))
                      rarg [||];
          r
      end
  | Cswitch(esel, index, ecases, _dbg) ->
      begin match self#emit_expr env esel with
        None -> None
      | Some rsel ->
          let rscases =
            Array.map (fun (case, _dbg) -> self#emit_sequence env case) ecases
          in
          let r = join_array env rscases in
          self#insert env (Iswitch(index,
                                   Array.map (fun (_, s) -> s#extract) rscases))
                      rsel [||];
          r
      end
  | Ccatch(_, [], e1) ->
      self#emit_expr env e1
  | Ccatch(rec_flag, handlers, body) ->
      let handlers =
        List.map (fun (nfail, ids, e2, dbg) ->
            let rs =
              List.map
                (fun (id, typ) ->
                  let r = self#regs_for typ in name_regs id r; r)
                ids in
            (nfail, ids, rs, e2, dbg))
          handlers
      in
      let env =
        (* Since the handlers may be recursive, and called from the body,
           the same environment is used for translating both the handlers and
           the body. *)
        List.fold_left (fun env (nfail, _ids, rs, _e2, _dbg) ->
            env_add_static_exception nfail rs env)
          env handlers
      in
      let (r_body, s_body) = self#emit_sequence env body in
      let translate_one_handler (nfail, ids, rs, e2, _dbg) =
        assert(List.length ids = List.length rs);
        let new_env =
          List.fold_left (fun env ((id, _typ), r) -> env_add id r env)
            env (List.combine ids rs)
        in
        let (r, s) = self#emit_sequence new_env e2 in
        (nfail, (r, s))
      in
      let l = List.map translate_one_handler handlers in
      let a = Array.of_list ((r_body, s_body) :: List.map snd l) in
      let r = join_array env a in
      let aux (nfail, (_r, s)) = (nfail, s#extract) in
      self#insert env (Icatch (rec_flag, List.map aux l, s_body#extract))
        [||] [||];
      r
  | Cexit (nfail,args) ->
      begin match self#emit_parts_list env args with
        None -> None
      | Some (simple_list, ext_env) ->
          let src = self#emit_tuple ext_env simple_list in
          let dest_args =
            try env_find_static_exception nfail env
            with Not_found ->
              Misc.fatal_error ("Selection.emit_expr: unbound label "^
                                Stdlib.Int.to_string nfail)
          in
          (* Intermediate registers to handle cases where some
             registers from src are present in dest *)
          let tmp_regs = Reg.createv_like src in
          (* Ccatch registers must not contain out of heap pointers *)
          Array.iter (fun reg -> assert(reg.typ <> Addr)) src;
          self#insert_moves env src tmp_regs ;
          self#insert_moves env tmp_regs (Array.concat dest_args) ;
          self#insert env (Iexit nfail) [||] [||];
          None
      end
  | Ctrywith(e1, v, e2, _dbg) ->
      let (r1, s1) = self#emit_sequence env e1 in
      let rv = self#regs_for typ_val in
      let (r2, s2) = self#emit_sequence (env_add v rv env) e2 in
      let r = join env r1 s1 r2 s2 in
      self#insert env
        (Itrywith(s1#extract,
                  instr_cons (Iop Imove) [|Proc.loc_exn_bucket|] rv
                             (s2#extract)))
        [||] [||];
      r

method private emit_sequence (env:environment) exp =
  let s = {< instr_seq = dummy_instr >} in
  let r = s#emit_expr env exp in
  (r, s)

method private bind_let (env:environment) v r1 =
  if all_regs_anonymous r1 then begin
    name_regs v r1;
    env_add v r1 env
  end else begin
    let rv = Reg.createv_like r1 in
    name_regs v rv;
    self#insert_moves env r1 rv;
    env_add v rv env
  end

method private bind_let_mut (env:environment) v k r1 =
  let rv = self#regs_for k in
  name_regs v rv;
  self#insert_moves env r1 rv;
  env_add ~mut:Mutable v rv env

(* The following two functions, [emit_parts] and [emit_parts_list], force
   right-to-left evaluation order as required by the Flambda [Un_anf] pass
   (and to be consistent with the bytecode compiler). *)

method private emit_parts (env:environment) ~effects_after exp =
  let module EC = Effect_and_coeffect in
  let may_defer_evaluation =
    let ec = self#effects_of exp in
    match EC.effect_ ec with
    | Effect.Arbitrary | Effect.Raise ->
      (* Preserve the ordering of effectful expressions by evaluating them
         early (in the correct order) and assigning their results to
         temporaries.  We can avoid this in just one case: if we know that
         every [exp'] in the original expression list (cf. [emit_parts_list])
         to be evaluated after [exp] cannot possibly affect the result of
         [exp] or depend on the result of [exp], then [exp] may be deferred.
         (Checking purity here is not enough: we need to check copurity too
         to avoid e.g. moving mutable reads earlier than the raising of
         an exception.) *)
      EC.pure_and_copure effects_after
    | Effect.None ->
      match EC.coeffect ec with
      | Coeffect.None ->
        (* Pure expressions may be moved. *)
        true
      | Coeffect.Read_mutable -> begin
        (* Read-mutable expressions may only be deferred if evaluation of
           every [exp'] (for [exp'] as in the comment above) has no effects
           "worse" (in the sense of the ordering in [Effect.t]) than raising
           an exception. *)
        match EC.effect_ effects_after with
        | Effect.None | Effect.Raise -> true
        | Effect.Arbitrary -> false
      end
      | Coeffect.Arbitrary -> begin
        (* Arbitrary expressions may only be deferred if evaluation of
           every [exp'] (for [exp'] as in the comment above) has no effects. *)
        match EC.effect_ effects_after with
        | Effect.None -> true
        | Effect.Arbitrary | Effect.Raise -> false
      end
  in
  (* Even though some expressions may look like they can be deferred from
     the (co)effect analysis, it may be forbidden to move them. *)
  if may_defer_evaluation && self#is_simple_expr exp then
    Some (exp, env)
  else begin
    match self#emit_expr env exp with
      None -> None
    | Some r ->
        if Array.length r = 0 then
          Some (Ctuple [], env)
        else begin
          (* The normal case *)
          let id = V.create_local "bind" in
          if all_regs_anonymous r then
            (* r is an anonymous, unshared register; use it directly *)
            Some (Cvar id, env_add (VP.create id) r env)
          else begin
            (* Introduce a fresh temp to hold the result *)
            let tmp = Reg.createv_like r in
            self#insert_moves env r tmp;
            Some (Cvar id, env_add (VP.create id) tmp env)
          end
        end
  end

method private emit_parts_list (env:environment) exp_list =
  let module EC = Effect_and_coeffect in
  let exp_list_right_to_left, _effect =
    (* Annotate each expression with the (co)effects that happen after it
       when the original expression list is evaluated from right to left.
       The resulting expression list has the rightmost expression first. *)
    List.fold_left (fun (exp_list, effects_after) exp ->
        let exp_effect = self#effects_of exp in
        (exp, effects_after)::exp_list, EC.join exp_effect effects_after)
      ([], EC.none)
      exp_list
  in
  List.fold_left (fun results_and_env (exp, effects_after) ->
      match results_and_env with
      | None -> None
      | Some (result, env) ->
          match self#emit_parts env exp ~effects_after with
          | None -> None
          | Some (exp_result, env) -> Some (exp_result :: result, env))
    (Some ([], env))
    exp_list_right_to_left

method private emit_tuple_not_flattened env exp_list =
  let rec emit_list = function
    [] -> []
  | exp :: rem ->
      (* Again, force right-to-left evaluation *)
      let loc_rem = emit_list rem in
      match self#emit_expr env exp with
        None -> assert false  (* should have been caught in emit_parts *)
      | Some loc_exp -> loc_exp :: loc_rem
  in
  emit_list exp_list

method private emit_tuple env exp_list =
  Array.concat (self#emit_tuple_not_flattened env exp_list)

method emit_extcall_args env ty_args args =
  let args = self#emit_tuple_not_flattened env args in
  let ty_args =
    if ty_args = [] then List.map (fun _ -> XInt) args else ty_args in
  let locs, stack_ofs = Proc.loc_external_arguments ty_args in
  let ty_args = Array.of_list ty_args in
  if stack_ofs <> 0 then
    self#insert env (Iop(Istackoffset stack_ofs)) [||] [||];
  List.iteri
    (fun i arg ->
      self#insert_move_extcall_arg env ty_args.(i) arg locs.(i))
    args;
  Array.concat (Array.to_list locs), stack_ofs

method insert_move_extcall_arg env _ty_arg src dst =
  (* The default implementation is one or two ordinary moves.
     (Two in the case of an int64 argument on a 32-bit platform.)
     It can be overridden to use special move instructions,
     for example a "32-bit move" instruction for int32 arguments. *)
  self#insert_moves env src dst

method emit_stores env data regs_addr =
  let a =
    ref (Arch.offset_addressing Arch.identity_addressing (-Arch.size_int)) in
  List.iter
    (fun e ->
      let (op, arg) = self#select_store false !a e in
      match self#emit_expr env arg with
        None -> assert false
      | Some regs ->
          match op with
            Istore(_, _, _) ->
              for i = 0 to Array.length regs - 1 do
                let r = regs.(i) in
                let kind = if r.typ = Float then Double else Word_val in
                self#insert env
                            (Iop(Istore(kind, !a, false)))
                            (Array.append [|r|] regs_addr) [||];
                a := Arch.offset_addressing !a (size_component r.typ)
              done
          | _ ->
              self#insert env (Iop op) (Array.append regs regs_addr) [||];
              a := Arch.offset_addressing !a (size_expr env e))
    data

(* Same, but in tail position *)

method private emit_return (env:environment) exp =
  match self#emit_expr env exp with
    None -> ()
  | Some r ->
      let loc = Proc.loc_results (Reg.typv r) in
      self#insert_moves env r loc;
      self#insert env Ireturn loc [||]

method emit_tail (env:environment) exp =
  match exp with
    Clet(v, e1, e2) ->
      begin match self#emit_expr env e1 with
        None -> ()
      | Some r1 -> self#emit_tail (self#bind_let env v r1) e2
      end
  | Clet_mut (v, k, e1, e2) ->
     begin match self#emit_expr env e1 with
       None -> ()
     | Some r1 -> self#emit_tail (self#bind_let_mut env v k r1) e2
     end
  | Cphantom_let (_var, _defining_expr, body) ->
      self#emit_tail env body
  | Cop((Capply ty) as op, args, dbg) ->
      begin match self#emit_parts_list env args with
        None -> ()
      | Some(simple_args, env) ->
          let (new_op, new_args) = self#select_operation op simple_args dbg in
          match new_op with
            Icall_ind ->
              let r1 = self#emit_tuple env new_args in
              let rarg = Array.sub r1 1 (Array.length r1 - 1) in
              let (loc_arg, stack_ofs) = Proc.loc_arguments (Reg.typv rarg) in
              if stack_ofs = 0 then begin
                let call = Iop (Itailcall_ind) in
                self#insert_moves env rarg loc_arg;
                self#insert_debug env call dbg
                            (Array.append [|r1.(0)|] loc_arg) [||];
              end else begin
                let rd = self#regs_for ty in
                let loc_res = Proc.loc_results (Reg.typv rd) in
                self#insert_move_args env rarg loc_arg stack_ofs;
                self#insert_debug env (Iop new_op) dbg
                            (Array.append [|r1.(0)|] loc_arg) loc_res;
                self#insert env (Iop(Istackoffset(-stack_ofs))) [||] [||];
                self#insert env Ireturn loc_res [||]
              end
          | Icall_imm { func; } ->
              let r1 = self#emit_tuple env new_args in
              let (loc_arg, stack_ofs) = Proc.loc_arguments (Reg.typv r1) in
              if stack_ofs = 0 then begin
                let call = Iop (Itailcall_imm { func; }) in
                self#insert_moves env r1 loc_arg;
                self#insert_debug env call dbg loc_arg [||];
              end else if func = !current_function_name then begin
                let call = Iop (Itailcall_imm { func; }) in
                let loc_arg' = Proc.loc_parameters (Reg.typv r1) in
                self#insert_moves env r1 loc_arg';
                self#insert_debug env call dbg loc_arg' [||];
              end else begin
                let rd = self#regs_for ty in
                let loc_res = Proc.loc_results (Reg.typv rd) in
                self#insert_move_args env r1 loc_arg stack_ofs;
                self#insert_debug env (Iop new_op) dbg loc_arg loc_res;
                self#insert env (Iop(Istackoffset(-stack_ofs))) [||] [||];
                self#insert env Ireturn loc_res [||]
              end
          | _ -> Misc.fatal_error "Selection.emit_tail"
      end
  | Csequence(e1, e2) ->
      begin match self#emit_expr env e1 with
        None -> ()
      | Some _ -> self#emit_tail env e2
      end
  | Cifthenelse(econd, _ifso_dbg, eif, _ifnot_dbg, eelse, _dbg) ->
      let (cond, earg) = self#select_condition econd in
      begin match self#emit_expr env earg with
        None -> ()
      | Some rarg ->
          self#insert env
                      (Iifthenelse(cond, self#emit_tail_sequence env eif,
                                         self#emit_tail_sequence env eelse))
                      rarg [||]
      end
  | Cswitch(esel, index, ecases, _dbg) ->
      begin match self#emit_expr env esel with
        None -> ()
      | Some rsel ->
          let cases =
            Array.map (fun (case, _dbg) -> self#emit_tail_sequence env case)
              ecases
          in
          self#insert env (Iswitch (index, cases)) rsel [||]
      end
  | Ccatch(_, [], e1) ->
      self#emit_tail env e1
  | Ccatch(rec_flag, handlers, e1) ->
      let handlers =
        List.map (fun (nfail, ids, e2, dbg) ->
            let rs =
              List.map
                (fun (id, typ) ->
                  let r = self#regs_for typ in name_regs id r; r)
                ids in
            (nfail, ids, rs, e2, dbg))
          handlers in
      let env =
        List.fold_left (fun env (nfail, _ids, rs, _e2, _dbg) ->
            env_add_static_exception nfail rs env)
          env handlers in
      let s_body = self#emit_tail_sequence env e1 in
      let aux (nfail, ids, rs, e2, _dbg) =
        assert(List.length ids = List.length rs);
        let new_env =
          List.fold_left
            (fun env ((id, _typ),r) -> env_add id r env)
            env (List.combine ids rs) in
        nfail, self#emit_tail_sequence new_env e2
      in
      self#insert env (Icatch(rec_flag, List.map aux handlers, s_body))
        [||] [||]
  | Ctrywith(e1, v, e2, _dbg) ->
      let (opt_r1, s1) = self#emit_sequence env e1 in
      let rv = self#regs_for typ_val in
      let s2 = self#emit_tail_sequence (env_add v rv env) e2 in
      self#insert env
        (Itrywith(s1#extract,
                  instr_cons (Iop Imove) [|Proc.loc_exn_bucket|] rv s2))
        [||] [||];
      begin match opt_r1 with
        None -> ()
      | Some r1 ->
          let loc = Proc.loc_results (Reg.typv r1) in
          self#insert_moves env r1 loc;
          self#insert env Ireturn loc [||]
      end
  | Cop _
  | Cconst_int _ | Cconst_natint _ | Cconst_float _ | Cconst_symbol _
  | Cvar _
  | Creturn_addr
  | Cassign _
  | Ctuple _
  | Cexit _ ->
    self#emit_return env exp

method private emit_tail_sequence env exp =
  let s = {< instr_seq = dummy_instr >} in
  s#emit_tail env exp;
  s#extract

(* Sequentialization of a function definition *)

method emit_fundecl ~future_funcnames f =
  current_function_name := f.Cmm.fun_name;
  let rargs =
    List.map
      (fun (id, ty) -> let r = self#regs_for ty in name_regs id r; r)
      f.Cmm.fun_args in
  let rarg = Array.concat rargs in
  let loc_arg = Proc.loc_parameters (Reg.typv rarg) in
  let env =
    List.fold_right2
      (fun (id, _ty) r env -> env_add id r env)
      f.Cmm.fun_args rargs env_empty in
  self#emit_tail env f.Cmm.fun_body;
  let body = self#extract in
  instr_seq <- dummy_instr;
  self#insert_moves env loc_arg rarg;
  let polled_body =
    if Polling.requires_prologue_poll ~future_funcnames
         ~fun_name:f.Cmm.fun_name body
      then
        instr_cons_debug
          (Iop(Ipoll { return_label = None })) [||] [||] f.Cmm.fun_dbg body
    else
      body
    in
  let body_with_prologue = self#extract_onto polled_body in
  { fun_name = f.Cmm.fun_name;
    fun_args = loc_arg;
    fun_body = body_with_prologue;
    fun_codegen_options = f.Cmm.fun_codegen_options;
    fun_dbg  = f.Cmm.fun_dbg;
    fun_poll = f.Cmm.fun_poll;
    fun_num_stack_slots = Array.make Proc.num_register_classes 0;
  }

end

let reset () =
  current_function_name := ""
