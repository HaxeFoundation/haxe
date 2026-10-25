(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*  Florian Angeletti, projet Cambium, INRIA Paris                        *)
(*                                                                        *)
(*   Copyright 2024 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Printing functions *)


open Types

type namespace := Shape.Sig_component_kind.t

val namespaced_ident: namespace -> Ident.t -> string
val string_of_path: Path.t -> string
val strings_of_paths: namespace -> Path.t list -> string list
(** Print a list of paths, using the same naming context to
    avoid name collisions *)

(** [printed_signature sourcefile ppf sg] print the signature [sg] of
        [sourcefile] with potential warnings for name collisions *)
val printed_signature: string -> Format.formatter -> signature -> unit

module type Printers := sig

    val wrap_printing_env: error:bool -> Env.t -> (unit -> 'a) -> 'a
    (** Call the function using the environment for type path shortening This
        affects all the printing functions below Also, if [~error:true], then
        disable the loading of cmis *)

    type 'a printer
    val longident: Longident.t printer
    val ident: Ident.t printer
    val path: Path.t printer
    val type_path: Path.t printer
    (** Print a type path taking account of [-short-paths].
        Calls should be within [wrap_printing_env]. *)


    (** Print out a type. This will pick names for type variables, and will not
        reuse names for common type variables shared across multiple type
        expressions. (It will also reset the printing state, which matters for
        other type formatters such as [prepared_type_expr].) If you want
        multiple types to use common names for type variables, see
        {!Out_type.prepare_for_printing} and {!Out_type.prepared_type_expr}. *)
    val type_expr: type_expr printer

    val type_scheme: type_expr printer

    val shared_type_scheme: type_expr printer
    (** [shared_type_scheme] is very similar to [type_scheme], but does not
        reset the printing context first. This is intended to be used in cases
        where the printing should have a particularly wide context, such as
        documentation generators; most use cases, such as error messages, have
        narrower contexts for which [type_scheme] is better suited. *)

    val type_expansion:
      Out_type.type_or_scheme -> Errortrace.expanded_type printer

    val label : label_declaration printer

    val constructor : constructor_declaration printer
    val constructor_arguments: constructor_arguments printer

    val extension_constructor:
      Ident.t -> extension_constructor printer
    (** Prints extension constructor with the type signature:
         type ('a, 'b) bar += A of float
    *)

    val extension_only_constructor:
      Ident.t -> extension_constructor printer
    (** Prints only extension constructor without type signature:
         A of float
    *)


    val value_description: Ident.t -> value_description printer
    val type_declaration: Ident.t -> type_declaration printer
    val modtype_declaration: Ident.t -> modtype_declaration printer
    val class_declaration: Ident.t -> class_declaration printer
    val cltype_declaration: Ident.t -> class_type_declaration printer


    val modtype: module_type printer
    val signature: signature printer
    val class_type: class_type printer

  end

module Doc : Printers with type 'a printer := 'a Format_doc.printer

(** For compatibility with Format printers *)
include Printers with type 'a printer := 'a Format_doc.format_printer
