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

(* Emission of Intel x86_64 assembly code *)

open Cmm
open Arch
open Proc
open Reg
open Mach
open Linear
open Emitaux
open Emitenv

open X86_ast
open X86_proc
open X86_dsl
module String = Misc.Stdlib.String
module Int = Numbers.Int

(* [Branch_relaxation] is not used in this file, but is required by
   emit.mlp files for certain other targets; the reference here ensures
   that when releases are being prepared the .depend files are correct
   for all targets. *)
[@@@ocaml.warning "-66"]
open! Branch_relaxation

let _label s = D.label ~typ:QWORD s

(* Override proc.ml *)

let int_reg_name =
  [| RAX; RBX; RDI; RSI; RDX; RCX; R8; R9;
     R12; R13; R10; R11; RBP; |]

let float_reg_name = Array.init 16 (fun i -> XMM i)

let register_name r =
  if r < 100 then Reg64 (int_reg_name.(r))
  else Regf (float_reg_name.(r - 100))

(* CFI directives *)

let cfi_startproc () =
  if Config.asm_cfi_supported then D.cfi_startproc ()

let cfi_endproc () =
  if Config.asm_cfi_supported then D.cfi_endproc ()

let cfi_adjust_cfa_offset n =
  if Config.asm_cfi_supported then D.cfi_adjust_cfa_offset n

let cfi_remember_state () =
  if Config.asm_cfi_supported then D.cfi_remember_state ()

let cfi_restore_state () =
  if Config.asm_cfi_supported then D.cfi_restore_state ()

let cfi_def_cfa_register reg =
  if Config.asm_cfi_supported then D.cfi_def_cfa_register reg

let emit_debug_info dbg =
  emit_debug_info_gen dbg D.file D.loc

let fp = Config.with_frame_pointers

let stack_threshold_size = Config.stack_threshold * 8 (* bytes *)

let frame_size env =                     (* includes return address *)
  if env.f.fun_frame_required then
    env.stack_offset
    + 8 * (env.f.fun_num_stack_slots.(0) + env.f.fun_num_stack_slots.(1))
    + 8
    + (if fp then 8 else 0)
  else
    env.stack_offset + 8

let slot_offset env loc cl =
  match loc with
  | Incoming n -> frame_size env + n
  | Local n ->
      if cl = 0
      then env.stack_offset + n * 8
      else env.stack_offset + (env.f.fun_num_stack_slots.(0) + n) * 8
  | Outgoing n -> n
  | Domainstate _ -> assert false  (* not a stack slot *)

(* Symbols *)

let symbol_prefix = if system = S_macosx then "_" else ""

let emit_symbol s = string_of_symbol symbol_prefix s

(* Record symbols used and defined - at the end generate extern for those
   used but not defined *)

let symbols_defined = ref String.Set.empty
let symbols_used = ref String.Set.empty

let add_def_symbol s = symbols_defined := String.Set.add s !symbols_defined
let add_used_symbol s = symbols_used := String.Set.add s !symbols_used

let imp_table = Hashtbl.create 16

let reset_imp_table () = Hashtbl.clear imp_table

let get_imp_symbol s =
  match Hashtbl.find imp_table s with
  | exception Not_found ->
      let imps = "__caml_imp_" ^ s in
      Hashtbl.add imp_table s imps;
      imps
  | imps -> imps

let emit_imp_table () =
  let f s imps =
    _label (emit_symbol imps);
    D.qword (ConstLabel (emit_symbol s))
  in
  D.data();
  D.comment "relocation table start";
  D.align 8;
  Hashtbl.iter f imp_table;
  D.comment "relocation table end"

let mem__imp s =
  let imp_s = get_imp_symbol s in
  mem64_rip QWORD (emit_symbol imp_s)

let rel_plt s =
  if windows && !Clflags.dlcode then mem__imp s
  else
    sym (if use_plt then emit_symbol s ^ "@PLT" else emit_symbol s)

let emit_call s = I.call (rel_plt s)

let emit_jump s = I.jmp (rel_plt s)

let load_symbol_addr s arg =
  if !Clflags.dlcode then
    if windows then begin
      (* I.mov (mem__imp s) arg (\* mov __caml_imp_foo(%rip), ... *\) *)
      I.mov (sym (emit_symbol s)) arg (* movabsq $foo, ... *)
    end else I.mov (mem64_rip QWORD (emit_symbol s ^ "@GOTPCREL")) arg
  else if !Clflags.pic_code then
    I.lea (mem64_rip NONE (emit_symbol s)) arg
  else
    I.mov (sym (emit_symbol s)) arg

let domain_field f =
  mem64 QWORD (Domainstate.idx_of_field f * 8) R14

(* Output a label *)

let emit_label lbl =
  match system with
  | S_macosx | S_win64 -> "L" ^ Int.to_string lbl
  | _ -> ".L" ^ Int.to_string lbl

let label s = sym (emit_label s)

let def_label ?typ s =
  D.label ?typ (emit_label s)

let emit_Llabel env fallthrough lbl =
  if not fallthrough && env.f.fun_fast then D.align 4;
  def_label lbl

(* Output a pseudo-register *)

let x86_data_type_for_stack_slot = function
  | Float -> REAL8
  | _ -> QWORD

let reg env = function
  | { loc = Reg.Reg r } -> register_name r
  | { loc = Stack(Domainstate n); typ = ty } ->
      let ofs = n + Domainstate.(idx_of_field Domain_extra_params) * 8 in
      mem64 (x86_data_type_for_stack_slot ty) ofs R14
  | { loc = Stack s; typ = ty } as r ->
      let ofs = slot_offset env s (register_class r) in
      mem64 (x86_data_type_for_stack_slot ty) ofs RSP
  | { loc = Unknown } ->
      assert false

let reg64 = function
  | { loc = Reg.Reg r } -> int_reg_name.(r)
  | _ -> assert false

let arg env i n = reg env i.arg.(n)
let res env i n = reg env i.res.(n)

(* Output a reference to the lower 8, 16 or 32 bits of a register *)

let reg_low_8_name  = Array.map (fun r -> Reg8L r) int_reg_name
let reg_low_16_name = Array.map (fun r -> Reg16 r) int_reg_name
let reg_low_32_name = Array.map (fun r -> Reg32 r) int_reg_name

let emit_subreg env tbl typ r =
  match r.loc with
  | Reg.Reg r when r < 13 -> tbl.(r)
  | Stack s -> mem64 typ (slot_offset env s (register_class r)) RSP
  | _ -> assert false

let arg64 i n = reg64 i.arg.(n)

(* Output an addressing mode *)

let addressing addr typ i n =
  match addr with
  | Ibased(s, ofs) ->
      add_used_symbol s;
      mem64_rip typ (emit_symbol s) ~ofs
  | Iindexed d ->
      mem64 typ d (arg64 i n)
  | Iindexed2 d ->
      mem64 typ ~base:(arg64 i n) d (arg64 i (n+1))
  | Iscaled(2, d) ->
      mem64 typ ~base:(arg64 i n) d (arg64 i n)
  | Iscaled(scale, d) ->
      mem64 typ ~scale d (arg64 i n)
  | Iindexed2scaled(scale, d) ->
      mem64 typ ~scale ~base:(arg64 i n) d (arg64 i (n+1))

(* Record live pointers at call points -- see Emitaux *)

let record_frame_label env live dbg =
  let lbl = new_label () in
  let live_offset = ref [] in
  Reg.Set.iter
    (function
      | {typ = Val; loc = Reg r} ->
          live_offset := ((r lsl 1) + 1) :: !live_offset
      | {typ = Val; loc = Stack s} as reg ->
          live_offset := slot_offset env s (register_class reg) :: !live_offset
      | {typ = Addr} as r ->
          Misc.fatal_error ("bad GC root " ^ Reg.name r)
      | _ -> ()
    )
    live;
  record_frame_descr ~label:lbl ~frame_size:(frame_size env)
    ~live_offset:!live_offset dbg;
  lbl

let record_frame env live dbg =
  let lbl = record_frame_label env live dbg in
  def_label lbl

let emit_call_gc gc =
  def_label gc.gc_lbl;
  emit_call "caml_call_gc";
  def_label gc.gc_frame_lbl;
  I.jmp (label gc.gc_return_lbl)

let bound_error_label env dbg =
  if !Clflags.debug then begin
    let lbl_bound_error = new_label() in
    let lbl_frame = record_frame_label env Reg.Set.empty (Dbg_other dbg) in
    env.bound_error_sites <- { bd_lbl = lbl_bound_error; bd_frame = lbl_frame; }
                             :: env.bound_error_sites;
    lbl_bound_error
  end else begin
    match env.bound_error_call with
    | None ->
      let lbl = new_label () in
      env.bound_error_call <- Some lbl;
      lbl
    | Some lbl -> lbl
  end

let emit_call_bound_error bd =
  def_label bd.bd_lbl;
  emit_call "caml_ml_array_bound_error";
  def_label bd.bd_frame

let emit_call_bound_errors env =
  List.iter emit_call_bound_error env.bound_error_sites;
  match env.bound_error_call with
  | Some lbl ->
    def_label lbl;
    emit_call "caml_ml_array_bound_error"
  | None -> ()

(* Names for instructions *)

let instr_for_intop = function
  | Iadd -> I.add
  | Isub -> I.sub
  | Imul -> (fun arg1 arg2 -> I.imul arg1 (Some arg2))
  | Iand -> I.and_
  | Ior -> I.or_
  | Ixor -> I.xor
  | Ilsl -> I.sal
  | Ilsr -> I.shr
  | Iasr -> I.sar
  | _ -> assert false

let instr_for_floatop = function
  | Iaddf -> I.addsd
  | Isubf -> I.subsd
  | Imulf -> I.mulsd
  | Idivf -> I.divsd
  | _ -> assert false

let instr_for_floatarithmem = function
  | Ifloatadd -> I.addsd
  | Ifloatsub -> I.subsd
  | Ifloatmul -> I.mulsd
  | Ifloatdiv -> I.divsd

let cond = function
  | Isigned Ceq   -> E   | Isigned Cne   -> NE
  | Isigned Cle   -> LE  | Isigned Cgt   -> G
  | Isigned Clt   -> L   | Isigned Cge   -> GE
  | Iunsigned Ceq -> E   | Iunsigned Cne -> NE
  | Iunsigned Cle -> BE  | Iunsigned Cgt -> A
  | Iunsigned Clt -> B   | Iunsigned Cge -> AE

(* Output an = 0 or <> 0 test. *)

let output_test_zero env arg =
  match arg.loc with
  | Reg.Reg _ -> I.test (reg env arg) (reg env arg)
  | _  -> I.cmp (int 0) (reg env arg)

(* Output a floating-point compare and branch *)

let emit_float_test env cmp i lbl =
  let arg = arg env in
  (* Effect of comisd on flags and conditional branches:
                     ZF PF CF  cond. branches taken
        unordered     1  1  1  je, jb, jbe, jp
        >             0  0  0  jne, jae, ja
        <             0  0  1  jne, jbe, jb
        =             1  0  0  je, jae, jbe.
     If FP traps are on (they are off by default),
     comisd traps on QNaN and SNaN but ucomisd traps on SNaN only.
  *)
  match cmp with
  | CFeq ->
      let next = new_label() in
      I.ucomisd (arg i 1) (arg i 0);
      I.jp (label next);          (* skip if unordered *)
      I.je lbl;                   (* branch taken if x=y *)
      def_label next
  | CFneq ->
      I.ucomisd (arg i 1) (arg i 0);
      I.jp lbl;                   (* branch taken if unordered *)
      I.jne lbl                   (* branch taken if x<y or x>y *)
  | CFlt ->
      I.comisd (arg i 0) (arg i 1);
      I.ja lbl                    (* branch taken if y>x i.e. x<y *)
  | CFnlt ->
      I.comisd (arg i 0) (arg i 1);
      I.jbe lbl                   (* taken if unordered or y<=x i.e. !(x<y) *)
  | CFle ->
      I.comisd (arg i 0) (arg i 1);(* swap compare *)
      I.jae lbl                    (* branch taken if y>=x i.e. x<=y *)
  | CFnle ->
      I.comisd (arg i 0) (arg i 1);(* swap compare *)
      I.jb lbl                     (* taken if unordered or y<x i.e. !(x<=y) *)
  | CFgt ->
      I.comisd (arg i 1) (arg i 0);
      I.ja lbl                     (* branch taken if x>y *)
  | CFngt ->
      I.comisd (arg i 1) (arg i 0);
      I.jbe lbl                    (* taken if unordered or x<=y i.e. !(x>y) *)
  | CFge ->
      I.comisd (arg i 1) (arg i 0);(* swap compare *)
      I.jae lbl                    (* branch taken if x>=y *)
  | CFnge ->
      I.comisd (arg i 1) (arg i 0);(* swap compare *)
      I.jb lbl                     (* taken if unordered or x<y i.e. !(x>=y) *)

(* Deallocate the stack frame before a return or tail call *)

let output_epilogue env f =
  if env.f.fun_frame_required then begin
    let n = (frame_size env) - 8 - (if fp then 8 else 0) in
    if n <> 0
    then begin
      I.add (int n) rsp;
      cfi_adjust_cfa_offset (-n);
    end;
    if fp then I.pop rbp;
    f ();
    (* reset CFA back cause function body may continue *)
    if n <> 0
    then cfi_adjust_cfa_offset n
  end
  else
    f ()

(* Floating-point constants *)

let float_constants = ref ([] : (int64 * int) list)

let add_float_constant cst =
  try
    List.assoc cst !float_constants
  with Not_found ->
    let lbl = new_label() in
    float_constants := (cst, lbl) :: !float_constants;
    lbl

let emit_float_constant f lbl =
  _label (emit_label lbl);
  D.qword (Const f)

let emit_global_label s =
  let lbl = Compilenv.make_symbol (Some s) in
  add_def_symbol lbl;
  let lbl = emit_symbol lbl in
  D.global lbl;
  _label lbl

(* Output .text section directive, or named .text.caml.<name> if enabled and
   supported on the target system. *)

let emit_named_text_section func_name =
  if !Clflags.function_sections then
    begin match system with
    | S_macosx
    (* Names of section segments in macosx are restricted to 16 characters,
       but function names are often longer, especially anonymous functions. *)
    | S_win64 | S_mingw64 | S_cygwin
    (* Win systems provide named text sections, but configure on these
       systems does not support function sections. *)
      ->  assert false
    | _ -> D.section
             [ ".text.caml."^(emit_symbol func_name) ]
             (Some "ax")
             ["@progbits"]
    end
  else D.text ()

(* Output the assembly code for an instruction *)

(* Emit an instruction *)
let emit_instr env fallthrough i =
  let arg8 i n = emit_subreg env reg_low_8_name BYTE i.arg.(n) in
  let arg16 i n = emit_subreg env reg_low_16_name WORD i.arg.(n) in
  let arg32 i n = emit_subreg env reg_low_32_name DWORD i.arg.(n) in
  let res16 i n = emit_subreg env reg_low_16_name WORD i.res.(n) in
  let res32 i n = emit_subreg env reg_low_32_name DWORD i.res.(n) in
  let arg = arg env in
  let res = res env in
  emit_debug_info i.dbg;
  match i.desc with
  | Lend -> ()
  | Lprologue ->
    if fp then begin
      I.push rbp;
      cfi_adjust_cfa_offset 8;
      I.mov rsp rbp;
    end;
    if env.f.fun_frame_required then begin
      let n = (frame_size env) - 8 - (if fp then 8 else 0) in
      if n <> 0
      then begin
        I.sub (int n) rsp;
        cfi_adjust_cfa_offset n;
      end;
    end
  | Lop(Imove | Ispill | Ireload) ->
      let src = i.arg.(0) and dst = i.res.(0) in
      if src.loc <> dst.loc then
        begin match src.typ, src.loc, dst.loc with
        | Float, Reg.Reg _, Reg.Reg _ -> I.movapd (reg env src) (reg env dst)
        | Float, _, _ -> I.movsd (reg env src) (reg env dst)
        | _ -> I.mov (reg env src) (reg env dst)
        end
  | Lop(Iconst_int n) ->
      if n = 0n then begin
        match i.res.(0).loc with
        | Reg _ ->
          (* Clearing the bottom half also clears the top half (except for
             64-bit-only registers where the behaviour is as if the operands
             were 64 bit). *)
          I.xor (res32 i 0) (res32 i 0)
        | _ ->
          I.mov (int 0) (res i 0)
      end else if n > 0n && n <= 0xFFFF_FFFFn then begin
        match i.res.(0).loc with
        | Reg _ ->
          (* Similarly, setting only the bottom half clears the top half. *)
          I.mov (nat n) (res32 i 0)
        | _ ->
          I.mov (nat n) (res i 0)
      end else
        I.mov (nat n) (res i 0)
  | Lop(Iconst_float f) ->
      begin match f with
      | 0x0000_0000_0000_0000L ->       (* +0.0 *)
          I.xorpd (res i 0) (res i 0)
      | _ ->
          let lbl = add_float_constant f in
          I.movsd (mem64_rip NONE (emit_label lbl)) (res i 0)
      end
  | Lop(Iconst_symbol s) ->
      add_used_symbol s;
      load_symbol_addr s (res i 0)
  | Lop(Icall_ind) ->
      I.call (arg i 0);
      record_frame env i.live (Dbg_other i.dbg)
  | Lop(Icall_imm { func; }) ->
      add_used_symbol func;
      emit_call func;
      record_frame env i.live (Dbg_other i.dbg)
  | Lop(Itailcall_ind) ->
      output_epilogue env (fun () -> I.jmp (arg i 0))
  | Lop(Itailcall_imm { func; }) ->
      begin
        if func = env.f.fun_name then
          I.jmp (label env.f.fun_tailrec_entry_point_label)
        else begin
          output_epilogue env begin fun () ->
            add_used_symbol func;
            emit_jump func
          end
        end
      end
  | Lop(Iextcall { func; alloc; stack_ofs }) ->
      add_used_symbol func;
      if stack_ofs > 0 then begin
        I.mov rsp r13;
        I.lea (mem64 QWORD stack_ofs RSP) r12;
        load_symbol_addr func rax;
        emit_call "caml_c_call_stack_args";
        record_frame env i.live (Dbg_other i.dbg);
      end else if alloc then begin
        load_symbol_addr func rax;
        emit_call "caml_c_call";
        record_frame env i.live (Dbg_other i.dbg);
      end else begin
        I.mov rsp rbx;
        cfi_remember_state ();
        cfi_def_cfa_register "rbx";
        (* NB: gdb has asserts on contiguous stacks that mean it
           will not unwind through this unless we were to tag this
           calling frame with cfi_signal_frame in it's definition. *)
        I.mov (domain_field Domainstate.Domain_c_stack) rsp;

        emit_call func;
        I.mov rbx rsp;
        cfi_restore_state ();
      end
  | Lop(Istackoffset n) ->
      if n < 0
      then I.add (int (-n)) rsp
      else if n > 0
      then I.sub (int n) rsp;
      if n <> 0
      then cfi_adjust_cfa_offset n;
      env.stack_offset <- env.stack_offset + n
  | Lop(Iload { memory_chunk; addressing_mode; _ }) ->
      let dest = res i 0 in
      begin match memory_chunk with
      | Word_int | Word_val ->
          I.mov (addressing addressing_mode QWORD i 0) dest
      | Byte_unsigned ->
          I.movzx (addressing addressing_mode BYTE i 0) dest
      | Byte_signed ->
          I.movsx (addressing addressing_mode BYTE i 0) dest
      | Sixteen_unsigned ->
          I.movzx (addressing addressing_mode WORD i 0) dest
      | Sixteen_signed ->
          I.movsx (addressing addressing_mode WORD i 0) dest;
      | Thirtytwo_unsigned ->
          I.mov (addressing addressing_mode DWORD i 0) (res32 i 0)
      | Thirtytwo_signed ->
          I.movsxd (addressing addressing_mode DWORD i 0) dest
      | Single ->
          I.cvtss2sd (addressing addressing_mode REAL4 i 0) dest
      | Double ->
          I.movsd (addressing addressing_mode REAL8 i 0) dest
      end
  | Lop(Istore(chunk, addr, _)) ->
      begin match chunk with
      | Word_int | Word_val ->
          I.mov (arg i 0) (addressing addr QWORD i 1)
      | Byte_unsigned | Byte_signed ->
          I.mov (arg8 i 0) (addressing addr BYTE i 1)
      | Sixteen_unsigned | Sixteen_signed ->
          I.mov (arg16 i 0) (addressing addr WORD i 1)
      | Thirtytwo_signed | Thirtytwo_unsigned ->
          I.mov (arg32 i 0) (addressing addr DWORD i 1)
      | Single ->
          I.cvtsd2ss (arg i 0) xmm15;
          I.movss xmm15 (addressing addr REAL4 i 1)
      | Double ->
          I.movsd (arg i 0) (addressing addr REAL8 i 1)
      end
  | Lop(Ialloc { bytes = n; dbginfo }) ->
      assert (n <= (Config.max_young_wosize + 1) * Arch.size_addr);
      if env.f.fun_fast then begin
        I.sub (int n) r15;
        I.cmp (domain_field Domainstate.Domain_young_limit) r15;
        let lbl_call_gc = new_label() in
        let lbl_frame =
          record_frame_label env i.live (Dbg_alloc dbginfo)
        in
        I.jb (label lbl_call_gc);
        let lbl_after_alloc = new_label() in
        def_label lbl_after_alloc;
        I.lea (mem64 NONE 8 R15) (res i 0);
        env.call_gc_sites <-
          { gc_lbl = lbl_call_gc;
            gc_return_lbl = lbl_after_alloc;
            gc_frame_lbl = lbl_frame; } :: env.call_gc_sites
      end else begin
        begin match n with
        | 16 -> emit_call "caml_alloc1"
        | 24 -> emit_call "caml_alloc2"
        | 32 -> emit_call "caml_alloc3"
        | _  ->
            I.sub (int n) r15;
            emit_call "caml_allocN"
        end;
        let label = record_frame_label env i.live (Dbg_alloc dbginfo) in
        def_label label;
        I.lea (mem64 NONE 8 R15) (res i 0)
      end
  | Lop(Ipoll { return_label }) ->
      I.cmp (domain_field Domainstate.Domain_young_limit) r15;
      let gc_call_label = new_label () in
      let lbl_after_poll = match return_label with
                  | None -> new_label()
                  | Some(lbl) -> lbl in
      let lbl_frame =
        record_frame_label env i.live (Dbg_alloc [])
      in
      begin match return_label with
      | None -> I.jbe (label gc_call_label)
      | Some return_label -> I.ja (label return_label)
      end;
      env.call_gc_sites <-
        { gc_lbl = gc_call_label;
          gc_return_lbl = lbl_after_poll;
          gc_frame_lbl = lbl_frame; } :: env.call_gc_sites;
      begin match return_label with
      | None -> def_label lbl_after_poll
      | Some _ -> I.jmp (label gc_call_label)
      end
  | Lop(Iintop(Icomp cmp)) ->
      I.cmp (arg i 1) (arg i 0);
      I.set (cond cmp) al;
      I.movzx al (res i 0)
  | Lop(Iintop_imm(Icomp cmp, n)) ->
      I.cmp (int n) (arg i 0);
      I.set (cond cmp) al;
      I.movzx al (res i 0)
  | Lop(Iintop (Icheckbound)) ->
      let lbl = bound_error_label env i.dbg in
      I.cmp (arg i 1) (arg i 0);
      I.jbe (label lbl)
  | Lop(Iintop_imm(Icheckbound, n)) ->
      let lbl = bound_error_label env i.dbg in
      I.cmp (int n) (arg i 0);
      I.jbe (label lbl)
  | Lop(Iintop(Idiv | Imod)) ->
      I.cqo ();
      I.idiv (arg i 1)
  | Lop(Iintop(Ilsl | Ilsr | Iasr as op)) ->
      (* We have i.arg.(0) = i.res.(0) and i.arg.(1) = %rcx *)
      instr_for_intop op cl (res i 0)
  | Lop(Iintop Imulh) ->
      I.imul (arg i 1) None
  | Lop(Iintop op) ->
      (* We have i.arg.(0) = i.res.(0) *)
      instr_for_intop op (arg i 1) (res i 0)
  | Lop(Iintop_imm(Iadd, n)) when i.arg.(0).loc <> i.res.(0).loc ->
      I.lea (mem64 NONE n (arg64 i 0)) (res i 0)
  | Lop(Iintop_imm(Iadd, 1) | Iintop_imm(Isub, -1)) ->
      I.inc (res i 0)
  | Lop(Iintop_imm(Iadd, -1) | Iintop_imm(Isub, 1)) ->
      I.dec (res i 0)
  | Lop(Iintop_imm(op, n)) ->
      (* We have i.arg.(0) = i.res.(0) *)
      instr_for_intop op (int n) (res i 0)
  | Lop(Icompf cmp) ->
      let cond, need_swap = float_cond_and_need_swap cmp in
      let a0, a1 = if need_swap then arg i 1, arg i 0 else arg i 0, arg i 1 in
      I.cmpsd cond a1 a0;
      I.movd a0 (res i 0);
      I.neg (res i 0)
  | Lop(Inegf) ->
      I.xorpd (mem64_rip OWORD (emit_symbol "caml_negf_mask")) (res i 0)
  | Lop(Iabsf) ->
      I.andpd (mem64_rip OWORD (emit_symbol "caml_absf_mask")) (res i 0)
  | Lop(Iaddf | Isubf | Imulf | Idivf as floatop) ->
      instr_for_floatop floatop (arg i 1) (res i 0)
  | Lop(Ifloatofint) ->
      I.cvtsi2sd  (arg i 0)  (res i 0)
  | Lop(Iintoffloat) ->
      I.cvttsd2si (arg i 0) (res i 0)
  | Lop(Iopaque) ->
      assert (i.arg.(0).loc = i.res.(0).loc)
  | Lop(Ispecific(Ilea addr)) ->
      I.lea (addressing addr NONE i 0) (res i 0)
  | Lop(Ispecific(Istore_int(n, addr, _))) ->
      I.mov (nat n) (addressing addr QWORD i 0)
  | Lop(Ispecific(Ioffset_loc(n, addr))) ->
      I.add (int n) (addressing addr QWORD i 0)
  | Lop(Ispecific(Ifloatarithmem(op, addr))) ->
      instr_for_floatarithmem op (addressing addr REAL8 i 1) (res i 0)
  | Lop(Ispecific(Ibswap 16)) ->
      I.xchg ah al;
      I.movzx (res16 i 0) (res i 0)
  | Lop(Ispecific(Ibswap 32)) ->
      I.bswap (res32 i 0);
      I.movsxd (res32 i 0) (res i 0)
  | Lop(Ispecific(Ibswap 64)) ->
      I.bswap (res i 0)
  | Lop(Ispecific(Ibswap _)) ->
      assert false
  | Lop(Ispecific Isqrtf) ->
      if arg i 0 <> res i 0 then
        I.xorpd (res i 0) (res i 0); (* avoid partial register stall *)
      I.sqrtsd (arg i 0) (res i 0)
  | Lop(Ispecific(Ifloatsqrtf addr)) ->
      I.xorpd (res i 0) (res i 0); (* avoid partial register stall *)
      I.sqrtsd (addressing addr REAL8 i 0) (res i 0)
  | Lop(Ispecific(Isextend32)) ->
      I.movsxd (arg32 i 0) (res i 0)
  | Lop(Ispecific(Izextend32)) ->
      I.mov (arg32 i 0) (res32 i 0)
  | Lop (Idls_get) ->
      I.mov (domain_field Domainstate.Domain_dls_root) (res i 0)
  | Lop (Ireturn_addr) ->
      let offset = frame_size env - 8 in
      I.mov (mem64 QWORD offset RSP) (res i 0)
  | Lreloadretaddr ->
      ()
  | Lreturn ->
      output_epilogue env begin fun () ->
        I.ret ()
      end
  | Llabel lbl ->
      emit_Llabel env fallthrough lbl
  | Lbranch lbl ->
      I.jmp (label lbl)
  | Lcondbranch(tst, lbl) ->
      let lbl = label lbl in
      begin match tst with
      | Itruetest ->
          output_test_zero env i.arg.(0);
          I.jne lbl
      | Ifalsetest ->
          output_test_zero env i.arg.(0);
          I.je lbl
      | Iinttest cmp ->
          I.cmp (arg i 1) (arg i 0);
          I.j (cond cmp) lbl
      | Iinttest_imm((Isigned Ceq | Isigned Cne |
                      Iunsigned Ceq | Iunsigned Cne) as cmp, 0) ->
          output_test_zero env i.arg.(0);
          I.j (cond cmp) lbl
      | Iinttest_imm(cmp, n) ->
          I.cmp (int n) (arg i 0);
          I.j (cond cmp) lbl
      | Ifloattest cmp ->
          emit_float_test env cmp i lbl
      | Ioddtest ->
          I.test (int 1) (arg8 i 0);
          I.jne lbl
      | Ieventest ->
          I.test (int 1) (arg8 i 0);
          I.je lbl
      end
  | Lcondbranch3(lbl0, lbl1, lbl2) ->
      I.cmp (int 1) (arg i 0);
      begin match lbl0 with
      | None -> ()
      | Some lbl -> I.jb (label lbl)
      end;
      begin match lbl1 with
      | None -> ()
      | Some lbl -> I.je (label lbl)
      end;
      begin match lbl2 with
      | None -> ()
      | Some lbl -> I.ja (label lbl)
      end
  | Lswitch jumptbl ->
      let lbl = emit_label (new_label()) in
      (* rax and rdx are clobbered by the Lswitch,
         meaning that no variable that is live across the Lswitch
         is assigned to rax or rdx.  However, the argument to Lswitch
         can still be assigned to one of these two registers, so
         we must be careful not to clobber it before use. *)
      let (tmp1, tmp2) =
        if i.arg.(0).loc = Reg 0 (* rax *)
        then (phys_reg 4 (*rdx*), phys_reg 0 (*rax*))
        else (phys_reg 0 (*rax*), phys_reg 4 (*rdx*)) in

      I.lea (mem64_rip NONE lbl) (reg env tmp1);
      I.movsxd (mem64 DWORD 0 (arg64 i 0) ~scale:4 ~base:(reg64 tmp1))
               (reg env tmp2);
      I.add (reg env tmp2) (reg env tmp1);
      I.jmp (reg env tmp1);

      begin match system with
      | S_mingw64 | S_cygwin -> D.section [".rdata"] (Some "dr") []
      | S_macosx | S_win64 -> ()
        (* with LLVM/OS X and MASM, use the text segment *)
      | _ -> D.section [".rodata"] None []
      end;
      D.align 4;
      _label lbl;
      for i = 0 to Array.length jumptbl - 1 do
        D.long (ConstSub (ConstLabel(emit_label jumptbl.(i)),
                         ConstLabel lbl))
      done;
      emit_named_text_section env.f.fun_name
  | Lentertrap ->
      if fp then begin
        let delta = frame_size env - 16 (* retaddr + rbp *) in
        I.lea (mem64 NONE delta RSP) rbp
      end;
  | Ladjust_trap_depth { delta_traps; } ->
      (* each trap occupies 16 bytes on the stack *)
      let delta = 16 * delta_traps in
      cfi_adjust_cfa_offset delta;
      env.stack_offset <- env.stack_offset + delta
  | Lpushtrap { lbl_handler; } ->
      let load_label_addr s arg =
        if !Clflags.pic_code then
          I.lea (mem64_rip NONE (emit_label s)) arg
        else
          I.mov (sym (emit_label s)) arg
      in
      load_label_addr lbl_handler r11;
      I.push r11;
      cfi_adjust_cfa_offset 8;
      I.push (domain_field Domainstate.Domain_exn_handler);
      cfi_adjust_cfa_offset 8;
      I.mov rsp (domain_field Domainstate.Domain_exn_handler);
      env.stack_offset <- env.stack_offset + 16;
  | Lpoptrap ->
      I.pop (domain_field Domainstate.Domain_exn_handler);
      cfi_adjust_cfa_offset (-8);
      I.add (int 8) rsp;
      cfi_adjust_cfa_offset (-8);
      env.stack_offset <- env.stack_offset - 16
  | Lraise k ->
      begin match k with
      | Lambda.Raise_regular ->
          emit_call "caml_raise_exn";
          record_frame env Reg.Set.empty (Dbg_raise i.dbg)
      | Lambda.Raise_reraise ->
          emit_call "caml_reraise_exn";
          record_frame env Reg.Set.empty (Dbg_raise i.dbg)
      | Lambda.Raise_notrace ->
          I.mov (domain_field Domainstate.Domain_exn_handler) rsp;
          I.pop (domain_field Domainstate.Domain_exn_handler);
          I.pop r11;
          I.jmp r11
      end

let rec emit_all env fallthrough i =
  match i.desc with
  | Lend -> ()
  | _ ->
      emit_instr env fallthrough i;
      emit_all env (Linear.has_fallthrough i.desc) i.next

let all_functions = ref []


(* Emission of a function declaration *)

let fundecl fundecl =
  let env = mk_env fundecl in
  all_functions := fundecl :: !all_functions;
  emit_named_text_section fundecl.fun_name;
  D.align 16;
  add_def_symbol fundecl.fun_name;
  if system = S_macosx
  && not !Clflags.output_c_object
  && is_generic_function fundecl.fun_name
  then (* PR#4690 *)
    D.private_extern (emit_symbol fundecl.fun_name)
  else
    D.global (emit_symbol fundecl.fun_name);
  D.label (emit_symbol fundecl.fun_name);
  emit_debug_info fundecl.fun_dbg;
  cfi_startproc ();
  if !Clflags.runtime_variant = "d" then
    emit_call "caml_assert_stack_invariants";
  let max_frame_size =
    frame_size env + fundecl.fun_extra_stack_used in
  let handle_overflow =
    if fundecl.fun_contains_nontail_calls
    || max_frame_size >= stack_threshold_size then begin
      let overflow = new_label () and ret = new_label () in
      let threshold_offset = Domainstate.stack_ctx_words * 8 + stack_threshold_size in
      I.lea (mem64 NONE (-(max_frame_size + threshold_offset)) RSP) r10;
      I.cmp (domain_field Domainstate.Domain_current_stack) r10;
      I.jb (label overflow);
      def_label ret;
      Some (overflow, ret)
    end else None
  in
  emit_all env true fundecl.fun_body;
  List.iter emit_call_gc env.call_gc_sites;
  emit_call_bound_errors env;
  begin match handle_overflow with
  | None -> ()
  | Some (overflow,ret) -> begin
      def_label overflow;
      (* Pass the desired frame size on the stack, since all of the
        argument-passing registers may be in use.
        Also serves to align the stack properly before the call *)
      I.push (int (Config.stack_threshold + max_frame_size / 8));
      cfi_adjust_cfa_offset 8;
        (* measured in words *)
      emit_call "caml_call_realloc_stack";
      I.pop r10; (* ignored *)
      cfi_adjust_cfa_offset (-8);
      I.jmp (label ret)
    end
  end;
  if fundecl.fun_frame_required then begin
    let n = (frame_size env) - 8 - (if fp then 8 else 0) in
    if n <> 0
    then begin
      cfi_adjust_cfa_offset (-n);
    end;
  end;
  cfi_endproc ();
  begin match system with
  | S_gnu | S_linux ->
      D.type_ (emit_symbol fundecl.fun_name) "@function";
      D.size (emit_symbol fundecl.fun_name)
        (ConstSub (
            ConstThis,
            ConstLabel (emit_symbol fundecl.fun_name)))
  | _ -> ()
  end

(* Emission of data *)

let emit_item = function
  | Cglobal_symbol s -> D.global (emit_symbol s)
  | Cdefine_symbol s -> add_def_symbol s; _label (emit_symbol s)
  | Cint8 n -> D.byte (const n)
  | Cint16 n -> D.word (const n)
  | Cint32 n -> D.long (const_nat n)
  | Cint n -> D.qword (const_nat n)
  | Csingle f -> D.long  (Const (Int64.of_int32 (Int32.bits_of_float f)))
  | Cdouble f -> D.qword (Const (Int64.bits_of_float f))
  | Csymbol_address s -> add_used_symbol s; D.qword (ConstLabel (emit_symbol s))
  | Cstring s -> D.bytes s
  | Cskip n -> if n > 0 then D.space n
  | Calign n -> D.align n

let data l =
  D.data ();
  D.align 8;
  List.iter emit_item l

(* Beginning / end of an assembly file *)

let begin_assembly() =
  X86_proc.reset_asm_code ();
  reset_debug_info();                   (* PR#5603 *)
  reset_imp_table();
  float_constants := [];
  all_functions := [];
  if system = S_win64 then begin
    D.extrn "caml_call_gc" NEAR;
    D.extrn "caml_c_call" NEAR;
    D.extrn "caml_allocN" NEAR;
    D.extrn "caml_alloc1" NEAR;
    D.extrn "caml_alloc2" NEAR;
    D.extrn "caml_alloc3" NEAR;
    D.extrn "caml_ml_array_bound_error" NEAR;
    D.extrn "caml_raise_exn" NEAR;
    D.extrn "caml_call_realloc_stack" NEAR;
    D.extrn "caml_reraise_exn" NEAR;
    D.extrn "caml_c_call_stack_args" NEAR;
    D.extrn "caml_assert_stack_invariants" NEAR;
  end;


  if !Clflags.dlcode || Arch.win64 then begin
    (* from amd64.S; could emit these constants on demand *)
    begin match system with
    | S_macosx -> D.section ["__TEXT";"__literal16"] None ["16byte_literals"]
    | S_mingw64 | S_cygwin -> D.section [".rdata"] (Some "dr") []
    | S_win64 -> D.data ()
    | _ -> D.section [".rodata.cst16"] (Some "aM") ["@progbits";"16"]
    end;
    D.align 16;
    _label (emit_symbol "caml_negf_mask");
    D.qword (Const 0x8000000000000000L);
    D.qword (Const 0L);
    D.align 16;
    _label (emit_symbol "caml_absf_mask");
    D.qword (Const 0x7FFFFFFFFFFFFFFFL);
    D.qword (Const 0xFFFFFFFFFFFFFFFFL);
  end;

  D.data ();
  emit_global_label "data_begin";

  emit_named_text_section (Compilenv.make_symbol (Some "code_begin"));
  emit_global_label "code_begin";
  if system = S_macosx then I.nop (); (* PR#4690 *)
  ()

let end_assembly() =
  if !float_constants <> [] then begin
    begin match system with
    | S_macosx -> D.section ["__TEXT";"__literal8"] None ["8byte_literals"]
    | S_mingw64 | S_cygwin -> D.section [".rdata"] (Some "dr") []
    | S_win64 -> D.data ()
    | _ -> D.section [".rodata.cst8"] (Some "aM") ["@progbits";"8"]
    end;
    D.align 8;
    List.iter (fun (cst,lbl) -> emit_float_constant cst lbl) !float_constants
  end;

  emit_named_text_section (Compilenv.make_symbol (Some "code_end"));
  if system = S_macosx then I.nop ();
  (* suppress "ld warning: atom sorting error" *)

  emit_global_label "code_end";

  emit_imp_table();

  D.data ();
  D.qword (const 0);  (* PR#6329 *)
  emit_global_label "data_end";
  D.qword (const 0);

  D.align 8;                            (* PR#7591 *)
  emit_global_label "frametable";

  let setcnt = ref 0 in
  emit_frames
    { efa_code_label = (fun l -> D.qword (ConstLabel (emit_label l)));
      efa_data_label = (fun l -> D.qword (ConstLabel (emit_label l)));
      efa_8 = (fun n -> D.byte (const n));
      efa_16 = (fun n -> D.word (const n));
      efa_32 = (fun n -> D.long (const_32 n));
      efa_word = (fun n -> D.qword (const n));
      efa_align = D.align;
      efa_label_rel =
        (fun lbl ofs ->
           let c =
             ConstAdd (
               ConstSub(ConstLabel(emit_label lbl), ConstThis),
               const_32 ofs
             ) in
           if system = S_macosx then begin
             incr setcnt;
             let s = Printf.sprintf "L$set$%d" !setcnt in
             D.setvar (s, c);
             D.long (ConstLabel s)
           end else
             D.long c
        );
      efa_def_label = (fun l -> _label (emit_label l));
      efa_string = (fun s -> D.bytes (s ^ "\000"))
    };

  if system = S_linux || system = S_freebsd || system = S_netbsd || system = S_openbsd then begin
    let frametable = emit_symbol (Compilenv.make_symbol (Some "frametable")) in
    D.size frametable (ConstSub (ConstThis, ConstLabel frametable))
  end;

  if system = S_linux || system = S_freebsd then
    (* Mark stack as non-executable, PR#4564 *)
    D.section [".note.GNU-stack"] (Some "") [ "%progbits" ];

  if system = S_win64 then begin
    D.comment "External functions";
    String.Set.iter
      (fun s ->
         if not (String.Set.mem s !symbols_defined) then
           D.extrn (emit_symbol s) NEAR)
      !symbols_used;
    symbols_used := String.Set.empty;
    symbols_defined := String.Set.empty;
  end;

  let asm =
    if !Emitaux.create_asm_file then
      Some
        (
         (if X86_proc.masm then X86_masm.generate_asm
          else X86_gas.generate_asm) !Emitaux.output_channel
        )
    else
      None
  in
  X86_proc.generate_code asm
