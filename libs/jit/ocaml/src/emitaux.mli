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

(* Common functions for emitting assembly code *)

val output_channel: out_channel ref
val emit_string: string -> unit
val emit_int: int -> unit
val emit_nativeint: nativeint -> unit
val emit_int32: int32 -> unit
val emit_symbol: string -> unit
val emit_printf: ('a, out_channel, unit) format -> 'a
val emit_char: char -> unit
val emit_string_literal: string -> unit
val emit_string_directive: string -> string -> unit
val emit_bytes_directive: string -> string -> unit
val emit_float64_directive: string -> int64 -> unit
val emit_float64_split_directive: string -> int64 -> unit
val emit_float32_directive: string -> int32 -> unit

val reset : unit -> unit
val reset_debug_info: unit -> unit
val emit_debug_info: Debuginfo.t -> unit
val emit_debug_info_gen :
  Debuginfo.t ->
  (file_num:int -> file_name:string -> unit) ->
  (file_num:int -> line:int -> col:int -> unit) -> unit

type frame_debuginfo =
  | Dbg_alloc of Debuginfo.alloc_dbginfo
  | Dbg_raise of Debuginfo.t
  | Dbg_other of Debuginfo.t

val record_frame_descr :
  label:int ->              (* Return address *)
  frame_size:int ->         (* Size of stack frame *)
  live_offset:int list ->   (* Offsets/regs of live addresses *)
  frame_debuginfo ->        (* Location, if any *)
  unit

type emit_frame_actions =
  { efa_code_label: int -> unit;
    efa_data_label: int -> unit;
    efa_8: int -> unit;
    efa_16: int -> unit;
    efa_32: int32 -> unit;
    efa_word: int -> unit;
    efa_align: int -> unit;
    efa_label_rel: int -> int32 -> unit;
    efa_def_label: int -> unit;
    efa_string: string -> unit }

val emit_frames: emit_frame_actions -> unit

val is_generic_function: string -> bool

val cfi_startproc : unit -> unit
val cfi_endproc : unit -> unit
val cfi_adjust_cfa_offset : int -> unit
val cfi_offset : reg:int -> offset:int -> unit
val cfi_def_cfa_offset : int -> unit
val cfi_remember_state : unit -> unit
val cfi_restore_state : unit -> unit
val cfi_def_cfa_register: reg:int -> unit

val binary_backend_available: bool ref
    (** Is a binary backend available.  If yes, we don't need
        to generate the textual assembly file (unless the user
        request it with -S). *)

val create_asm_file: bool ref
    (** Are we actually generating the textual assembly file? *)

type error =
  | Stack_frame_too_large of int

exception Error of error
val report_error: error Format_doc.format_printer
val report_error_doc: error Format_doc.printer

val mk_env : Linear.fundecl -> Emitenv.per_function_env

(* Output .text section directive, or named .text.caml.<name> if enabled. *)
val emit_named_text_section : string -> char -> unit
