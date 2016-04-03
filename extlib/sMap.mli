(*
 * SMap - Polymorphic maps
 * Copyright (C) 2016 Nicolas Cannasse
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version,
 * with the special exception on linking described in file LICENSE.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA
 *)

(** Polymorphic Map.

	This is a polymorphic map, similar to standard library [Map] module
	but in a defunctorized style.
*)

type 'a t

val empty : 'a t
(** The empty map, using [compare] as key comparison function. *)

val is_empty : 'a t -> bool
(** returns true if the map is empty. *)

val add : string -> 'a -> 'a t -> 'a t
(** [add x y m] returns a map containing the same bindings as
    [m], plus a binding of [x] to [y]. If [x] was already bound
    in [m], its previous binding disappears. *)

val find : string -> 'a t -> 'a
(** [find x m] returns the current binding of [x] in [m],
    or raises [Not_found] if no such binding exists. *)

val remove : string -> 'a t -> 'a t
(** [remove x m] returns a map containing the same bindings as
    [m], except for [x] which is unbound in the returned map. *)

val mem : string -> 'a t -> bool
(** [mem x m] returns [true] if [m] contains a binding for [x],
    and [false] otherwise. *)

val exists : string -> 'a t -> bool
(** same as [mem]. *)

val iter : (string -> 'a -> unit) -> 'a t -> unit
(** [iter f m] applies [f] to all bindings in map [m].
    [f] receives the key as first argument, and the associated value
    as second argument. The order in which the bindings are passed to
    [f] is unspecified. Only current bindings are presented to [f]:
    bindings hidden by more recent bindings are not passed to [f]. *)

val map : ('a -> 'b) -> 'a t -> 'b t
(** [map f m] returns a map with same domain as [m], where the
    associated value [a] of all bindings of [m] has been
    replaced by the result of the application of [f] to [a].
    The order in which the associated values are passed to [f]
    is unspecified. *)

val mapi : (string -> 'a -> 'b) -> 'a t -> 'b t
(** Same as [map], but the function receives as arguments both the
    key and the associated value for each binding of the map. *)

val fold : ('a -> 'b -> 'b) -> 'a t -> 'b -> 'b
(** [fold f m a] computes [(f kN dN ... (f k1 d1 a)...)],
    where [k1 ... kN] are the keys of all bindings in [m],
    and [d1 ... dN] are the associated data.
    The order in which the bindings are presented to [f] is
    unspecified. *)

val foldi : (string -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
(** Same as [fold], but the function receives as arguments both the
    key and the associated value for each binding of the map. *)

