(*
 * Bitset - Efficient bit sets
 * Copyright (C) 2003 Nicolas Cannasse
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
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *)

(** Efficient bit sets.

 A bitset is an array of boolean values that can be accessed with indexes
 like an array but provides a better memory usage (divided by 8) for a
 very small speed trade-off. *)

type t

exception Negative_index of string
(** When a negative bit value is used for one of the BitSet functions,
 this exception is raised with the name of the function. *)

val empty : unit ->  t
(** Create an empty bitset of size 0, the bitset will automatically expand
 when needed. *)

val create : int -> t
(** Create an empty bitset with an initial size (in number of bits). *)

val copy : t -> t
(** Copy a bitset : further modifications of first one will not affect the
 copy. *)

val clone : t -> t
(** Same as [copy] *)

val set : t -> int -> unit
(** [set s n] sets the nth-bit in the bitset [s] to true. *)

val unset : t -> int -> unit
(** [unset s n] sets the nth-bit in the bitset [s] to false. *)

val put : t -> bool -> int -> unit
(** [put s v n] sets the nth-bit in the bitset [s] to [v]. *)

val toggle : t -> int -> unit
(** [toggle s n] changes the nth-bit value in the bitset [s]. *)

val is_set : t -> int -> bool
(** [is_set s n] returns true if nth-bit in the bitset [s] is set,
 or false otherwise. *)

val compare : t -> t -> int
(** [compare s1 s2] compares two bitsets. Highest bit indexes are
 compared first. *)

val equals : t -> t -> bool
(** [equals s1 s2] returns true if, and only if, all bits values in s1 are
  the same as in s2. *)

val count : t -> int
(** [count s] returns the number of bits set in the bitset [s]. *)

val enum : t -> int Enum.t
(** [enum s] returns an enumeration of bits which are set
  in the bitset [s]. *)

val intersect : t -> t -> unit
(** [intersect s t] sets [s] to the intersection of the sets [s] and [t]. *)

val unite : t -> t -> unit
(** [unite s t] sets [s] to the union of the sets [s] and [t]. *)

val differentiate : t -> t -> unit
(** [differentiate s t] removes the elements of [t] from [s]. *)

val differentiate_sym : t -> t -> unit
(** [differentiate_sym s t] sets [s] to the symmetrical difference of the
  sets [s] and [t]. *)

val inter : t -> t -> t
(** [inter s t] returns the intersection of sets [s] and [t]. *)

val union : t -> t -> t
(** [union s t] return the union of sets [s]  and [t]. *)

val diff : t -> t -> t
(** [diff s t] returns [s]-[t]. *)

val sym_diff : t -> t -> t
(** [sym_diff s t] returns the symmetrical difference of [s] and [t]. *)
