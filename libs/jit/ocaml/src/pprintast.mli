(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Hongbo Zhang (University of Pennsylvania)                  *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)


(** Pretty-printers for {!Parsetree}

  {b Warning:} this module is unstable and part of
  {{!Compiler_libs}compiler-libs}.

*)

type space_formatter = (unit, Format.formatter, unit) format

val longident : Format.formatter -> Longident.t -> unit
val constr : Format.formatter -> Longident.t -> unit

val expression : Format.formatter -> Parsetree.expression -> unit
val string_of_expression : Parsetree.expression -> string

val pattern: Format.formatter -> Parsetree.pattern -> unit

val core_type: Format.formatter -> Parsetree.core_type -> unit

val signature: Format.formatter -> Parsetree.signature -> unit
val structure: Format.formatter -> Parsetree.structure -> unit
val string_of_structure: Parsetree.structure -> string

val module_expr: Format.formatter -> Parsetree.module_expr -> unit

val toplevel_phrase : Format.formatter -> Parsetree.toplevel_phrase -> unit
val top_phrase: Format.formatter -> Parsetree.toplevel_phrase -> unit

val class_field: Format.formatter -> Parsetree.class_field -> unit
val class_type_field: Format.formatter -> Parsetree.class_type_field -> unit
val class_expr: Format.formatter -> Parsetree.class_expr -> unit
val class_type: Format.formatter -> Parsetree.class_type -> unit
val module_type: Format.formatter -> Parsetree.module_type -> unit
val structure_item: Format.formatter -> Parsetree.structure_item -> unit
val signature_item: Format.formatter -> Parsetree.signature_item -> unit
val binding: Format.formatter -> Parsetree.value_binding -> unit
val payload: Format.formatter -> Parsetree.payload -> unit

val tyvar_of_name : string -> string
  (** Turn a type variable name into a valid identifier, taking care of the
      special treatment required for the single quote character in second
      position, or for keywords by escaping them with \#. No-op on "_". *)

val tyvar: Format.formatter -> string -> unit
  (** Print a type variable name as a valid identifier, taking care of the
      special treatment required for the single quote character in second
      position, or for keywords by escaping them with \#. No-op on "_". *)

(** {!Format_doc} functions for error messages *)
module Doc:sig
  val longident: Longident.t Format_doc.printer
  val constr: Longident.t Format_doc.printer
  val tyvar: string Format_doc.printer

  (** Returns a format document if the expression reads nicely as the subject
      of a sentence in a error message. *)
  val nominal_exp : Parsetree.expression -> Format_doc.t option
end
