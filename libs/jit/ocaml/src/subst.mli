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

(** Substitutions *)

open Types


(**
   Substitutions are used to translate a type from one context to
   another.  This requires substituting paths for identifiers, and
   possibly also lowering the level of non-generic variables so that
   they are inferior to the maximum level of the new context.

   Substitutions can also be used to create a "clean" copy of a type.
   Indeed, non-variable node of a type are duplicated, with their
   levels set to generic level.  That way, the resulting type is
   well-formed (decreasing levels), even if the original one was not.

   In the presence of local substitutions for module types, a substitution for a
   type expression may fail to produce a well-formed type. In order to confine
   this issue to local substitutions, the type of substitutions is split into a
   safe and unsafe variant. Only unsafe substitutions may expand a module type
   path into a generic module type. *)

(** Type familly for substitutions *)
type +'k subst

type safe = [`Safe]
type unsafe = [`Unsafe]

type t = safe subst
(** Standard substitution*)

val identity: 'a subst
val unsafe: t -> unsafe subst

val add_type: Ident.t -> Path.t -> 'k subst -> 'k subst
val add_module: Ident.t -> Path.t -> 'k subst -> 'k subst
val add_modtype: Ident.t -> Path.t -> 'k subst -> 'k subst

val for_saving: t -> t
val reset_for_saving: unit -> unit
val change_locs: 'k subst -> Location.t -> 'k subst

val module_path: t -> Path.t -> Path.t
val type_path: t -> Path.t -> Path.t
val modtype_path: t -> Path.t -> Path.t

val type_expr: t -> type_expr -> type_expr
val class_type: t -> class_type -> class_type
val value_description: t -> value_description -> value_description
val type_declaration: t -> type_declaration -> type_declaration
val extension_constructor:
        t -> extension_constructor -> extension_constructor
val class_declaration: t -> class_declaration -> class_declaration
val cltype_declaration: t -> class_type_declaration -> class_type_declaration

(**
   When applied to a signature item, a substitution not only modifies the types
   present in its declaration, but also refreshes the identifier of the item.
   Effectively this creates new declarations, and so one should decide what the
   scope of this new declaration should be.

   This is decided by the [scoping] argument passed to the following functions.
*)

type scoping =
  | Keep
  | Make_local
  | Rescope of int

val modtype: scoping -> t -> module_type -> module_type
val signature: scoping -> t -> signature -> signature
val signature_item: scoping -> t -> signature_item -> signature_item
val modtype_declaration:
  scoping -> t -> modtype_declaration -> modtype_declaration
val module_declaration: scoping -> t -> module_declaration -> module_declaration

(** Composition of substitutions:
     apply (compose s1 s2) x = apply s2 (apply s1 x) **)
val compose: t -> t -> t

module Unsafe: sig

  type t = unsafe subst
  (** Unsafe substitutions introduced by [with] constraints, local substitutions
      ([type t := int * int]) or recursive module check. *)

(** Replacing a module type name S by a non-path signature is unsafe as the
    packed module type [(module S)] becomes ill-formed. *)
  val add_modtype: Ident.t -> module_type -> 'any subst -> t
  val add_modtype_path: Path.t -> module_type -> 'any subst -> t

  (** Deep editing inside a module type require to retypecheck the module, for
      applicative functors in path and module aliases. *)
  val add_type_path: Path.t -> Path.t -> t -> t
  val add_type_function:
    Path.t -> params:type_expr list -> body:type_expr -> t -> t
  val add_module_path: Path.t -> Path.t -> t -> t

  type error =
    | Fcm_type_substituted_away of Path.t * Types.module_type

  type 'a res := ('a, error) result

  val type_declaration:  t -> type_declaration -> type_declaration res
  val signature_item: scoping -> t -> signature_item -> signature_item res
  val signature: scoping -> t -> signature -> signature res

  val compose: t -> t -> t res
  (** Composition of substitutions is eager and fails when the two substitution
      are incompatible, for example [ module type t := sig end] is not
      compatible with [module type s := sig type t=(module t) end]*)

end

module Lazy : sig
  type module_decl =
    {
      mdl_type: modtype;
      mdl_attributes: Parsetree.attributes;
      mdl_loc: Location.t;
      mdl_uid: Uid.t;
    }

  and modtype =
    | MtyL_ident of Path.t
    | MtyL_signature of signature
    | MtyL_functor of functor_parameter * modtype
    | MtyL_alias of Path.t

  and modtype_declaration =
    {
      mtdl_type: modtype option;  (* Note: abstract *)
      mtdl_attributes: Parsetree.attributes;
      mtdl_loc: Location.t;
      mtdl_uid: Uid.t;
    }

  and signature

  and signature_item =
      SigL_value of Ident.t * value_description * visibility
    | SigL_type of Ident.t * type_declaration * rec_status * visibility
    | SigL_typext of Ident.t * extension_constructor * ext_status * visibility
    | SigL_module of
        Ident.t * module_presence * module_decl * rec_status * visibility
    | SigL_modtype of Ident.t * modtype_declaration * visibility
    | SigL_class of Ident.t * class_declaration * rec_status * visibility
    | SigL_class_type of Ident.t * class_type_declaration *
                           rec_status * visibility

  and functor_parameter =
    | Unit
    | Named of Ident.t option * modtype


  val of_module_decl : Types.module_declaration -> module_decl
  val of_modtype : Types.module_type -> modtype
  val of_modtype_decl : Types.modtype_declaration -> modtype_declaration
  val of_signature : Types.signature -> signature
  val of_signature_items : signature_item list -> signature
  val of_signature_item : Types.signature_item -> signature_item

  val module_decl : scoping -> t -> module_decl -> module_decl
  val modtype : scoping -> t -> modtype -> modtype
  val modtype_decl : scoping -> t -> modtype_declaration -> modtype_declaration
  val signature : scoping -> t -> signature -> signature
  val signature_item : scoping -> t -> signature_item -> signature_item

  val force_module_decl : module_decl -> Types.module_declaration
  val force_modtype : modtype -> Types.module_type
  val force_modtype_decl : modtype_declaration -> Types.modtype_declaration
  val force_signature : signature -> Types.signature
  val force_signature_once : signature -> signature_item list
  val force_signature_item : signature_item -> Types.signature_item
end
