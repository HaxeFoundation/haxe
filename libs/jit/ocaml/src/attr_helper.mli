(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                  Jeremie Dimino, Jane Street Europe                    *)
(*                                                                        *)
(*   Copyright 2015 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Helpers for attributes

  {b Warning:} this module is unstable and part of
  {{!Compiler_libs}compiler-libs}.

*)

open Asttypes
open Parsetree

type error =
  | Multiple_attributes of string
  | No_payload_expected of string

(** The [string] argument of the following functions is the name of the
    attribute we are looking for.  If the argument is ["foo"], these functions
    will find attributes with the name ["foo"] or ["ocaml.foo"] *)
val get_no_payload_attribute : string -> attributes -> string loc option
val has_no_payload_attribute : string -> attributes -> bool

exception Error of Location.t * error

val report_error: error Format_doc.format_printer
val report_error_doc: error Format_doc.printer
