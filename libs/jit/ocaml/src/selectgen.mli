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

type environment

val env_add
   : ?mut:Asttypes.mutable_flag
  -> Backend_var.With_provenance.t
  -> Reg.t array
  -> environment
  -> environment

val env_find : Backend_var.t -> environment -> Reg.t array

val size_expr : environment -> Cmm.expression -> int

module Effect : sig
  type t =
    | None
    | Raise
    | Arbitrary
end

module Coeffect : sig
  type t =
    | None
    | Read_mutable
    | Arbitrary
end

module Effect_and_coeffect : sig
  type t

  val none : t
  val arbitrary : t

  val effect_ : t -> Effect.t
  val coeffect : t -> Coeffect.t

  val effect_only : Effect.t -> t
  val coeffect_only : Coeffect.t -> t

  val join : t -> t -> t
  val join_list_map : 'a list -> ('a -> t) -> t
end

class virtual selector_generic : object
  (* The following methods must or can be overridden by the processor
     description *)
  method is_immediate : Mach.integer_operation -> int -> bool
    (* Must be overridden to indicate whether a constant is a suitable
       immediate operand to the given integer arithmetic instruction.
       The default implementation handles shifts by immediate amounts,
       but produces no immediate operations otherwise. *)
  method virtual is_immediate_test : Mach.integer_comparison -> int -> bool
    (* Must be defined to indicate whether a constant is a suitable
       immediate operand to the given integer test *)
  method virtual select_addressing :
    Cmm.memory_chunk -> Cmm.expression -> Arch.addressing_mode * Cmm.expression
    (* Must be defined to select addressing modes *)
  method is_simple_expr: Cmm.expression -> bool
  method effects_of : Cmm.expression -> Effect_and_coeffect.t
    (* Can be overridden to reflect special extcalls known to be pure *)
  method select_operation :
    Cmm.operation ->
    Cmm.expression list ->
    Debuginfo.t ->
    Mach.operation * Cmm.expression list
    (* Can be overridden to deal with special arithmetic instructions *)
  method select_condition : Cmm.expression -> Mach.test * Cmm.expression
    (* Can be overridden to deal with special test instructions *)
  method select_store :
    bool -> Arch.addressing_mode -> Cmm.expression ->
                                         Mach.operation * Cmm.expression
    (* Can be overridden to deal with special store constant instructions *)
  method regs_for : Cmm.machtype -> Reg.t array
    (* Return an array of fresh registers of the given type.
       Default implementation is like Reg.createv.
       Can be overridden if float values are stored as pairs of
       integer registers. *)
  method insert_op :
    environment -> Mach.operation -> Reg.t array -> Reg.t array -> Reg.t array
    (* Can be overridden to deal with 2-address instructions
       or instructions with hardwired input/output registers *)
  method insert_op_debug :
    environment -> Mach.operation -> Debuginfo.t -> Reg.t array
      -> Reg.t array -> Reg.t array
    (* Can be overridden to deal with 2-address instructions
       or instructions with hardwired input/output registers *)
  method insert_move_extcall_arg :
    environment -> Cmm.exttype -> Reg.t array -> Reg.t array -> unit
    (* Can be overridden to deal with unusual unboxed calling conventions,
       e.g. on a 64-bit platform, passing unboxed 32-bit arguments
       in 32-bit stack slots. *)
  method emit_extcall_args :
    environment -> Cmm.exttype list -> Cmm.expression list -> Reg.t array * int
    (* Can be overridden to deal with stack-based calling conventions *)
  method emit_stores :
    environment -> Cmm.expression list -> Reg.t array -> unit
    (* Fill a freshly allocated block.  Can be overridden for architectures
       that do not provide Arch.offset_addressing. *)

  (* The following method is the entry point and should not be overridden *)
  method emit_fundecl : future_funcnames:Misc.Stdlib.String.Set.t
                                              -> Cmm.fundecl -> Mach.fundecl

  (* The following methods should not be overridden.  They cannot be
     declared "private" in the current implementation because they
     are not always applied to "self", but ideally they should be private. *)
  method extract_onto : Mach.instruction -> Mach.instruction
  method extract : Mach.instruction
  method insert :
    environment -> Mach.instruction_desc -> Reg.t array -> Reg.t array -> unit
  method insert_debug :
    environment -> Mach.instruction_desc -> Debuginfo.t ->
      Reg.t array -> Reg.t array -> unit
  method insert_move : environment -> Reg.t -> Reg.t -> unit
  method insert_move_args :
    environment -> Reg.t array -> Reg.t array -> int -> unit
  method insert_move_results :
    environment -> Reg.t array -> Reg.t array -> int -> unit
  method insert_moves : environment -> Reg.t array -> Reg.t array -> unit
  method emit_expr :
    environment -> Cmm.expression -> Reg.t array option
  method emit_tail : environment -> Cmm.expression -> unit
end

val reset : unit -> unit
