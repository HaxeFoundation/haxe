(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*  Florian Angeletti, projet Cambium, Inria Paris                        *)
(*                                                                        *)
(*   Copyright 2021 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Iterate on signature by syntactic group of items

    Classes, class types and private row types adds ghost components to
    the signature where they are defined.

    When editing or printing a signature it is therefore important to
    identify those ghost components.

    This module provides type grouping together ghost components
    with the corresponding core item (or recursive group) and
    the corresponding iterators.
*)

(** Classes and class types generate ghosts signature items, we group them
    together before printing *)
type sig_item =
  {
    src: Types.signature_item (** the syntactic item *)
;
    post_ghosts: Types.signature_item list
    (** ghost classes types are post-declared *);
  }

(** [flatten sig_item] is [x.src :: x.post_ghosts] *)
val flatten: sig_item -> Types.signature

(** A group of mutually recursive definition *)
type core_rec_group =
  | Not_rec of sig_item
  | Rec_group of sig_item list

(** [rec_items group] is the list of sig_items in the group *)
val rec_items: core_rec_group -> sig_item list

(** Private #row types are manifested as a sequence of definitions
    preceding a recursive group, we collect them and separate them from the
    syntactic recursive group. *)
type rec_group =
  { pre_ghosts: Types.signature_item list; group:core_rec_group }

(** The sequence [seq signature] iterates over [signature] {!rec_group} by
    {!rec_group}.
    The second element of the tuple in the {!full_seq} case is the not-yet
    traversed part of the signature.
*)
val next: Types.signature -> (rec_group * Types.signature) option
val seq: Types.signature -> rec_group Seq.t

val iter: (rec_group -> unit) -> Types.signature -> unit
val fold: ('acc -> rec_group -> 'acc) -> 'acc -> Types.signature -> 'acc

(** Describe how to amend one element of a signature *)
type in_place_patch = {
  ghosts: Types.signature; (** updated list of ghost items *)
  replace_by: Types.signature_item option;
  (** replacement for the selected item *)
}

(**
  [!replace_in_place patch sg] replaces the first element of the signature
   for which [patch ~ghosts component] returns [Some (value,patch)].
   The [ghosts] list is the current prefix of ghost components associated to
   [component]
*)
val replace_in_place:
  ( ghosts:Types.signature -> Types.signature_item
    -> ('a * in_place_patch) option )
  -> Types.signature -> ('a * Types.signature) option
