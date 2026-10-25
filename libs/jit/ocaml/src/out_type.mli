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

(** Functions for representing type expressions and module types as outcometree
    (with [as 'a] aliases for cycles) and printing them. All functions below
    depends on global contexts that keep track of

- If labels are disabled
- Current printing environment
- Shortest equivalent paths

- Conflicts for identifier names
- Names chosen for type variables
- Aliases used for representing cycles or row variables
- Uses of internal names

Whenever possible, it is advised to use the simpler functions available in
{!Printtyp} which take care of setting up this naming context. The functions
below are needed when one needs to share a common naming context (or part of it)
between different calls to printing functions (or in order to implement
{!Printtyp}).
*)

open Format_doc
open Types
open Outcometree

(** {1 Wrapping functions}*)

val wrap_printing_env: error:bool -> Env.t -> (unit -> 'a) -> 'a
(** Call the function using the environment for type path shortening
    This affects all the printing and tree cration functions functions below
    Also, if [~error:true], then disable the loading of cmis *)


(** [with_labels false] disable labels in function types *)
val with_labels: bool -> (unit -> 'a) -> 'a

(** {1 Printing idents and paths } *)

val ident_name: Shape.Sig_component_kind.t option -> Ident.t -> out_name
val tree_of_path: ?disambiguation:bool -> Path.t -> out_ident
val namespaced_tree_of_path: Shape.Sig_component_kind.t -> Path.t -> out_ident
val tree_of_type_path: Path.t -> out_ident
(** Specialized functions for printing types with [short-paths] *)

(** [same_path ty ty2] is true when there is an equation [ty]=[ty2] in the
    short-path scope*)
val same_path: type_expr -> type_expr -> bool

(** Simple heuristic to rewrite Foo__bar.* as Foo.Bar.* when Foo.Bar is an alias
   for Foo__bar. This pattern is used by the stdlib. *)
val rewrite_double_underscore_paths: Env.t -> Path.t -> Path.t

(** {1 Printing type expressions} *)

(** Printing type expressions requires to translate the internal graph based
    representation into to an {!Outcometree} closer to the source syntax. In
    order to do so, the printing is generally split in three phase:
     - A preparation phase which in particular
         - marks cycles
         - chooses user-facing names for type variables
     - An outcometree generation phase, where we emit an outcometree as a
     ready-for-printing representation of trees (represented by the various
     [tree_of_*] functions)
   - Printing proper
*)

(** [prepare_for_printing] resets the global naming environment, a la
    {!reset_except_conflicts}, and prepares the types for printing by reserving
    variable names and marking cycles. Any type variables that are shared
    between multiple types in the input list will be given the same name when
    printed with {!prepared_type_expr}. *)
val prepare_for_printing: type_expr list -> unit

(** [add_type_to_preparation ty] extend a previous type expression preparation
    to the type expression [ty]
*)
val add_type_to_preparation: type_expr -> unit

(** In [Type_scheme] mode, non-generic types variables are printed as weakly
    polymorphic type variables. *)
type type_or_scheme = Type | Type_scheme
val tree_of_typexp: type_or_scheme -> type_expr -> out_type
(** [tree_of_typexp] generate the [outcometree] for a prepared type
    expression.*)

val prepared_type_scheme: type_expr printer
val prepared_type_expr: type_expr printer
(** The printers [prepared_type_expr] and [prepared_type_scheme] should only be
    used on prepared types. Types can be prepared by initially calling
    {!prepare_for_printing} or adding them later to the preparation with
    {!add_type_to_preparation}.

    Calling this function on non-prepared types may cause a stack overflow (see
    #8860) due to cycles in the printed types.

    See {!Printtyp.type_expr} for a safer but less flexible printer. *)

(** [type_expr_with_reserved_names] can print "half-prepared" type expression. A
    "half-prepared" type expression should have had its names reserved (with
    {!Variable_names.reserve}), but should not have had its cycles marked. *)
val type_expr_with_reserved_names: type_expr printer

type 'a diff = Same of 'a | Diff of 'a * 'a
val trees_of_type_expansion:
  type_or_scheme -> Errortrace.expanded_type -> out_type diff
val prepare_expansion: Errortrace.expanded_type -> Errortrace.expanded_type
val pp_type_expansion: out_type diff printer
val hide_variant_name: Types.type_expr -> Types.type_expr


(** {1: Label and constructors }*)
val prepare_type_constructor_arguments: constructor_arguments -> unit
val tree_of_constructor_arguments: constructor_arguments -> out_type list

val tree_of_label: label_declaration -> out_label

val add_constructor_to_preparation : constructor_declaration -> unit
val prepared_constructor : constructor_declaration printer

val tree_of_extension_constructor:
    Ident.t -> extension_constructor -> ext_status -> out_sig_item
val extension_constructor_args_and_ret_type_subtree:
  constructor_arguments -> type_expr option -> out_type list * out_type option
val add_extension_constructor_to_preparation :
    extension_constructor -> unit
val prepared_extension_constructor:
    Ident.t -> extension_constructor printer


(** {1 Declarations }*)

val tree_of_type_declaration:
    Ident.t -> type_declaration -> rec_status -> out_sig_item
val add_type_declaration_to_preparation :
  Ident.t -> type_declaration -> unit
val prepared_type_declaration: Ident.t -> type_declaration printer

val tree_of_value_description: Ident.t -> value_description -> out_sig_item
val tree_of_modtype_declaration:
    Ident.t -> modtype_declaration -> out_sig_item
val tree_of_class_declaration:
    Ident.t -> class_declaration -> rec_status -> out_sig_item
val tree_of_cltype_declaration:
    Ident.t -> class_type_declaration -> rec_status -> out_sig_item

(** {1 Module types }*)

val tree_of_module:
    Ident.t -> ?ellipsis:bool -> module_type -> rec_status -> out_sig_item
val tree_of_modtype: module_type -> out_module_type
val tree_of_signature: Types.signature -> out_sig_item list

val tree_of_class_type: type_or_scheme -> class_type -> out_class_type
val prepare_class_type: class_type -> unit

(** {1 Toplevel printing}  *)
val print_items: (Env.t -> signature_item -> 'a option) ->
  Env.t -> signature_item list -> (out_sig_item * 'a option) list

(** {1 Naming contexts }*)

(** Path name, which were mutable at some point *)
module Out_name: sig
  val create: string -> out_name
  val print: out_name -> string
end

(** Disambiguation for identifiers, e.g. the two type constructors named [t]
in the type of [f] in
{[
  type t = A
  module M = struct
    type t = B
   let f A = B
  end
]}
should be disambiguated to [t/2->t] *)
module Ident_names: sig
  val enable: bool -> unit
  (** When contextual names are enabled, the mapping between identifiers
      and names is ensured to be one-to-one. *)

  (** [with_fuzzy id f] locally disable ident disambiguation for [id] within
      [f] *)
  val with_fuzzy: Ident.t -> (unit -> 'a) -> 'a
end

(** The [Ident_conflicts] module keeps track of conflicts arising when
    attributing names to identifiers and provides functions that can print
    explanations for these conflict in error messages *)
module Ident_conflicts: sig
  val exists: unit -> bool
  (** [exists()] returns true if the current naming context renamed
        an identifier to avoid a name collision *)

  type explanation =
    { kind: Shape.Sig_component_kind.t;
      name:string;
      root_name:string;
      location:Location.t
    }

  val list_explanations: unit -> explanation list
(** [list_explanations()] return the list of conflict explanations
    collected up to this point, and reset the list of collected
    explanations *)

  val print_located_explanations: explanation list printer

  val err_print: formatter -> unit
  val err_msg: unit -> doc option
  (** [err_msg ()] return an error message if there are pending conflict
      explanations at this point. It is often important to check for conflicts
      after all printing is done, thus the delayed nature of [err_msg]*)

  val reset: unit -> unit
end

(** Naming choice for type variable names (['a], ['b], ...), for instance the
    two classes of distinct type variables in
    {[let repeat x y = x, y, y, x]}
    should be printed printed as ['a -> 'b -> 'a * 'b * 'b * 'a].
*)
module Variable_names: sig

  (** Add external type equalities*)
  val add_subst: (type_expr * type_expr) list -> unit

  (** [reserve ty] registers the variable names appearing in [ty] *)
  val reserve: type_expr -> unit
end

(** Register internal typechecker names ([$0],[$a]) appearing in the
    [outcometree] *)
module Internal_names: sig
  val add: Path.t -> unit
  val reset: unit -> unit
  val print_explanations: Env.t -> formatter -> unit
end

(** Reset all contexts *)
val reset: unit -> unit

(** Reset all contexts except for conflicts *)
val reset_except_conflicts: unit -> unit
