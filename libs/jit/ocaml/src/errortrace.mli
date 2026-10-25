(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Florian Angeletti, projet Cambium, Inria Paris             *)
(*              Antal Spector-Zabusky, Jane Street, New York              *)
(*                                                                        *)
(*   Copyright 2018 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*   Copyright 2021 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open Types

type position = First | Second

val swap_position : position -> position
val print_pos : position Format_doc.printer

type expanded_type = { ty: type_expr; expanded: type_expr }

(** [trivial_expansion ty] creates an [expanded_type] whose expansion is also
    [ty].  Usually, you want [Ctype.expand_type] instead, since the expansion
    carries useful information; however, in certain circumstances, the error is
    about the expansion of the type, meaning that actually performing the
    expansion produces more confusing or inaccurate output. *)
val trivial_expansion : type_expr -> expanded_type

type 'a diff = { got: 'a; expected: 'a }

(** [map_diff f {expected;got}] is [{expected=f expected; got=f got}] *)
val map_diff: ('a -> 'b) -> 'a diff -> 'b diff

(** Scope escape related errors *)
type 'a escape_kind =
  | Constructor of Path.t
  | Univ of type_expr
  (* The type_expr argument of [Univ] is always a [Tunivar _],
     we keep a [type_expr] to track renaming in {!Printtyp} *)
  | Self
  | Module_type of Path.t
  | Equation of 'a
  | Constraint

type 'a escape =
  { kind : 'a escape_kind;
    context : type_expr option }

val map_escape : ('a -> 'b) -> 'a escape -> 'b escape

val explain: 'a list ->
  (prev:'a option -> 'a -> 'b option) ->
  'b option

(** Type indices *)
type unification = private Unification
type comparison  = private Comparison

type fixed_row_case =
  | Cannot_be_closed
  | Cannot_add_tags of string list

type 'variety variant =
  (* Common *)
  | Incompatible_types_for : string -> _ variant
  | No_tags : position * (Asttypes.label * row_field) list -> _ variant
  (* Unification *)
  | No_intersection : unification variant
  | Fixed_row :
      position * fixed_row_case * fixed_explanation -> unification variant
  (* Equality & Moregen *)
  | Presence_not_guaranteed_for : position * string -> comparison variant
  | Openness : position (* Always [Second] for Moregen *) -> comparison variant

type 'variety obj =
  (* Common *)
  | Missing_field : position * string -> _ obj
  | Abstract_row : position -> _ obj
  (* Unification *)
  | Self_cannot_be_closed : unification obj

type first_class_module =
    | Package_cannot_scrape of Path.t
    | Package_inclusion of Format_doc.doc
    | Package_coercion of Format_doc.doc

type ('a, 'variety) elt =
  (* Common *)
  | Diff : 'a diff -> ('a, _) elt
  | Variant : 'variety variant -> ('a, 'variety) elt
  | Obj : 'variety obj -> ('a, 'variety) elt
  | Escape : 'a escape -> ('a, _) elt
  | Function_label_mismatch of Asttypes.arg_label diff
  | Incompatible_fields : { name:string; diff: type_expr diff } -> ('a, _) elt
  | First_class_module: first_class_module -> ('a,_) elt
  (* Unification & Moregen; included in Equality for simplicity *)
  | Rec_occur : type_expr * type_expr -> ('a, _) elt

type ('a, 'variety) t = ('a, 'variety) elt list

type 'variety trace = (type_expr,     'variety) t
type 'variety error = (expanded_type, 'variety) t

val map : ('a -> 'b) -> ('a, 'variety) t -> ('b, 'variety) t

val incompatible_fields :
  name:string -> got:type_expr -> expected:type_expr -> (type_expr, _) elt

val swap_trace : ('a, 'variety) t -> ('a, 'variety) t

(** The traces (['variety t]) are the core error types.  However, we bundle them
    up into three "top-level" error types, which are used elsewhere:
    [unification_error], [equality_error], and [moregen_error].  In the case of
    [equality_error], this has to bundle in extra information; in general, it
    distinguishes the three types of errors and allows us to distinguish traces
    that are being built (or processed) from those that are complete and have
    become the final error.  These error types have the invariants that their
    traces are nonempty; we ensure that through three smart constructors with
    matching names. *)

type unification_error = private { trace : unification error } [@@unboxed]

type equality_error = private
  { trace : comparison error;
    subst : (type_expr * type_expr) list }

type moregen_error = private { trace : comparison error } [@@unboxed]

val unification_error : trace:unification error -> unification_error

val equality_error :
  trace:comparison error -> subst:(type_expr * type_expr) list -> equality_error

val moregen_error : trace:comparison error -> moregen_error

(** Wraps up the two different kinds of [comparison] errors in one type *)
type comparison_error =
  | Equality_error of equality_error
  | Moregen_error  of moregen_error

(** Lift [swap_trace] to [unification_error] *)
val swap_unification_error : unification_error -> unification_error

module Subtype : sig
  type 'a elt =
    | Diff of 'a diff

  type 'a t = 'a elt list

  (** Just as outside [Subtype], we split traces, completed traces, and complete
      errors.  However, in a minor asymmetry, the name [Subtype.error_trace]
      corresponds to the outside [error] type, and [Subtype.error] corresponds
      to the outside [*_error] types (e.g., [unification_error]).  This [error]
      type has the invariant that the subtype trace is nonempty; note that no
      such invariant is imposed on the unification trace. *)

  type trace       = type_expr t
  type error_trace = expanded_type t

  type unification_error_trace = unification error (** To avoid shadowing *)

  type nonrec error = private
    { trace             : error_trace
    ; unification_trace : unification error }

  val error :
    trace:error_trace -> unification_trace:unification_error_trace -> error

  val map : ('a -> 'b) -> 'a t -> 'b t
end
