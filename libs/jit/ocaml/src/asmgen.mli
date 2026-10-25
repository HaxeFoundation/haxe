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

(** From Lambda to assembly code *)

val compile_implementation_linear :
    Unit_info.t -> unit

val compile_phrase :
    ppf_dump:Format.formatter -> Cmm.phrase -> unit

type error =
  | Assembler_error of string
  | Mismatched_for_pack of string option
  | Asm_generation of string * Emitaux.error

exception Error of error
val report_error: error Format_doc.format_printer
val report_error_doc: error Format_doc.printer

val compile_unit
   : output_prefix:string
   -> asm_filename:string
   -> keep_asm:bool
   -> obj_filename:string
   -> (unit -> unit)
   -> unit
