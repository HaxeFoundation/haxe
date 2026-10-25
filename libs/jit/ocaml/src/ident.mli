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

(* Identifiers (unique names) *)

type t

include Identifiable.S with type t := t
(* Notes:
   - [equal] compares identifiers by name
   - [compare x y] is 0 if [same x y] is true.
   - [compare] compares identifiers by binding location
*)

val doc_print: t Format_doc.printer
val print_with_scope : t Format_doc.printer
        (** Same as {!print} except that it will also add a "[n]" suffix
            if the scope of the argument is [n]. *)


val create_scoped: scope:int -> string -> t
val create_local: string -> t
val create_persistent: string -> t
val create_predef: string -> t

val rename: t -> t
        (** Creates an identifier with the same name as the input, a fresh
            stamp, and no scope.
            @raise [Fatal_error] if called on a persistent / predef ident. *)

val name: t -> string
val unique_name: t -> string
val unique_toplevel_name: t -> string
val persistent: t -> bool
val same: t -> t -> bool
        (** Compare identifiers by binding location.
            Two identifiers are the same either if they are both
            non-persistent and have been created by the same call to
            [create_*], or if they are both persistent and have the same
            name. *)

val compare_stamp: t -> t -> int
        (** Compare only the internal stamps, 0 if absent *)

val compare: t -> t -> int
        (** Compare identifiers structurally, including the name *)

val global: t -> bool
val is_predef: t -> bool

val scope: t -> int

val lowest_scope : int
val highest_scope: int

val reinit: unit -> unit

type 'a tbl
(** ['a tbl] represents association tables from identifiers to values
   of type ['a].

   ['a tbl] plays the role of map, but bindings can be looked up
   from either the full Ident using [find_same], or just its
   user-visible name using [find_name]. In general the two lookups may
   not return the same result, as an identifier may have been shadowed
   in the environment by a distinct identifier with the same name.

   [find_all] returns the bindings for all idents of a given name,
   most recently introduced first.

   In other words,
     ['a tbl]
   corresponds to
     [(Ident.t * 'a) list Map.Make(String)]
   and the implementation is very close to that representation.

   Note in particular that searching among idents of the same name
   takes linear time, and that [add] simply extends the list without
   checking for duplicates. So it is not a good idea to implement
   union by repeated [add] calls, which may result in many duplicated
   identifiers and poor [find_same] performance. It is even possible
   to build overly large same-name lists such that non-recursive
   functions like [find_all] or [fold_all] blow the stack.

   You should probably use [Map.Make(Ident)] instead, unless you
   really need to query bindings by user-visible name, not just by
   unique identifiers.
*)

val empty: 'a tbl
val add: t -> 'a -> 'a tbl -> 'a tbl
val find_same: t -> 'a tbl -> 'a
val find_name: string -> 'a tbl -> t * 'a
val find_all: string -> 'a tbl -> (t * 'a) list
val find_all_seq: string -> 'a tbl -> (t * 'a) Seq.t
val fold_name: (t -> 'a -> 'b -> 'b) -> 'a tbl -> 'b -> 'b
val fold_all: (t -> 'a -> 'b -> 'b) -> 'a tbl -> 'b -> 'b
val iter: (t -> 'a -> unit) -> 'a tbl -> unit
val remove: t -> 'a tbl -> 'a tbl

(* Idents for sharing keys *)

val make_key_generator : unit -> (t -> t)
