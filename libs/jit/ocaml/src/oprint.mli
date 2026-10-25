(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                   Projet Cristal, INRIA Rocquencourt                   *)
(*                                                                        *)
(*   Copyright 2002 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open Outcometree

type 'a printer = 'a Format_doc.printer ref
type 'a toplevel_printer = (Format.formatter -> 'a -> unit) ref

val out_ident: out_ident printer
val out_value : out_value toplevel_printer
val out_label : out_label printer
val out_type : out_type printer
val out_type_args : out_type list printer
val out_constr : out_constructor printer
val out_class_type : out_class_type printer
val out_module_type : out_module_type printer
val out_sig_item : out_sig_item printer
val out_signature :out_sig_item list printer
val out_functor_parameters :
  (string option * Outcometree.out_module_type) option list printer
val out_type_extension : out_type_extension printer
val out_phrase : out_phrase toplevel_printer

val parenthesized_ident : string -> bool
