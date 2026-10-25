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
open Format_doc

type position = First | Second

let swap_position = function
  | First -> Second
  | Second -> First

let print_pos ppf = function
  | First -> fprintf ppf "first"
  | Second -> fprintf ppf "second"

type expanded_type = { ty: type_expr; expanded: type_expr }

let trivial_expansion ty = { ty; expanded = ty }

type 'a diff = { got: 'a; expected: 'a }

let map_diff f r =
  (* ordering is often meaningful when dealing with type_expr *)
  let got = f r.got in
  let expected = f r.expected in
  { got; expected }

let swap_diff x = { got = x.expected; expected = x.got }

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

let map_escape f esc =
  {esc with kind = match esc.kind with
     | Equation eq -> Equation (f eq)
     | (Constructor _ | Univ _ | Self | Module_type _ | Constraint) as c -> c}

let explain trace f =
  let rec explain = function
    | [] -> None
    | [h] -> f ~prev:None h
    | h :: (prev :: _ as rem) ->
      match f ~prev:(Some prev) h with
      | Some _ as m -> m
      | None -> explain rem in
  explain (List.rev trace)

(* Type indices *)
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
      (* Could move [Incompatible_fields] into [obj] *)
  | First_class_module: first_class_module -> ('a,_) elt
  (* Unification & Moregen; included in Equality for simplicity *)
  | Rec_occur : type_expr * type_expr -> ('a, _) elt

type ('a, 'variety) t = ('a, 'variety) elt list

type 'variety trace = (type_expr,     'variety) t
type 'variety error = (expanded_type, 'variety) t

let map_elt (type variety) f : ('a, variety) elt -> ('b, variety) elt = function
  | Diff x -> Diff (map_diff f x)
  | Escape {kind = Equation x; context} ->
      Escape { kind = Equation (f x); context }
  | Escape {kind = (Univ _ | Self | Constructor _ | Module_type _ | Constraint);
            _}
  | Variant _ | Obj _ | Function_label_mismatch _ | Incompatible_fields _
  | Rec_occur (_, _) | First_class_module _  as x -> x

let map f t = List.map (map_elt f) t

let incompatible_fields ~name ~got ~expected =
  Incompatible_fields { name; diff={got; expected} }

let swap_elt (type variety) : ('a, variety) elt -> ('a, variety) elt = function
  | Diff x -> Diff (swap_diff x)
  | Incompatible_fields { name; diff } ->
    Incompatible_fields { name; diff = swap_diff diff}
  | Obj (Missing_field(pos,s)) -> Obj (Missing_field(swap_position pos,s))
  | Obj (Abstract_row pos) -> Obj (Abstract_row (swap_position pos))
  | Variant (Fixed_row(pos,k,f)) ->
    Variant (Fixed_row(swap_position pos,k,f))
  | Variant (No_tags(pos,f)) ->
    Variant (No_tags(swap_position pos,f))
  | x -> x

let swap_trace e = List.map swap_elt e

type unification_error = { trace : unification error } [@@unboxed]

type equality_error =
  { trace : comparison error;
    subst : (type_expr * type_expr) list }

type moregen_error = { trace : comparison error } [@@unboxed]

let unification_error ~trace : unification_error =
  assert (trace <> []);
  { trace }

let equality_error ~trace ~subst : equality_error =
    assert (trace <> []);
    { trace; subst }

let moregen_error ~trace : moregen_error =
  assert (trace <> []);
  { trace }

type comparison_error =
  | Equality_error of equality_error
  | Moregen_error  of moregen_error

let swap_unification_error ({trace} : unification_error) =
  ({trace = swap_trace trace} : unification_error)

module Subtype = struct
  type 'a elt =
    | Diff of 'a diff

  type 'a t = 'a elt list

  type trace       = type_expr t
  type error_trace = expanded_type t

  type unification_error_trace = unification error (** To avoid shadowing *)

  type nonrec error =
    { trace             : error_trace
    ; unification_trace : unification error }

  let error ~trace ~unification_trace =
  assert (trace <> []);
  { trace; unification_trace }

  let map_elt f = function
    | Diff x -> Diff (map_diff f x)

  let map f t = List.map (map_elt f) t
end
