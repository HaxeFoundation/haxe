(*
 * RefList - List reference
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

(** Reference on lists.

  RefList is a extended set of functions that manipulate list
  references.
*)

exception Empty_list
exception Invalid_index of int

type 'a t

val empty : unit -> 'a t
(** Returns a new empty ref list *)
  
val is_empty : 'a t -> bool
(** Return [true] if a ref list is empty *)

val clear : 'a t -> unit
(** Removes all elements *)

val length : 'a t -> int
(** Returns the number of elements - O(n) *)

val copy : dst:'a t -> src:'a t -> unit
(** Makes a copy of a ref list - O(1) *)

val copy_list : dst:'a t -> src:'a list -> unit
(** Makes a copy of a list - O(1) *)

val copy_enum : dst:'a t -> src:'a Enum.t -> unit
(** Makes a copy of a enum *)

val of_list : 'a list -> 'a t
(** Creates a ref list from a list - O(1) *)

val to_list : 'a t -> 'a list
(** Returns the current elements as a list - O(1) *)

val of_enum : 'a Enum.t -> 'a t
(** Creates a ref list from an enumeration *)

val enum : 'a t -> 'a Enum.t
(** Returns an enumeration of current elements in the ref list *)

val add : 'a t -> 'a -> unit
(** Adds an element at the end - O(n) *)

val push : 'a t -> 'a -> unit
(** Adds an element at the head - O(1) *)

val add_sort : ?cmp:('a -> 'a -> int) -> 'a t -> 'a -> unit
(** Adds an element in a sorted list, using optional comparator
    or 'compare' as default. *)

val first : 'a t -> 'a
(** Returns the first element or
    raises [Empty_list] if the ref list is empty *)

val last : 'a t -> 'a
(** Returns the last element - O(n) or
    raises Empty_list if the ref list is empty *)

val pop : 'a t -> 'a
(** Removes and returns the first element or
   raises [Empty_list] if the ref list is empty *)

val npop : 'a t -> int -> 'a list
(** Removes and returns the n first elements or
   raises [Empty_list] if the ref list does not
   contain enough elements *)

val hd : 'a t -> 'a
(** same as [first] *)

val tl : 'a t -> 'a t
(** Returns a ref list containing the same elements
    but without the first one or
    raises [Empty_list] if the ref list is empty *)

val rev : 'a t -> unit
(** Reverses the ref list - O(n) *)

(** {6 Functional Operations} *)

val iter : ('a -> unit) -> 'a t -> unit
(** Apply the given function to all elements of the
    ref list, in respect with the order of the list *)

val find : ('a -> bool) -> 'a t -> 'a
(** Find the first element matching
    the specified predicate
    raise [Not_found] if no element is found *)

val rfind : ('a -> bool) -> 'a t -> 'a
(** Find the first element in the reversed ref list matching
    the specified predicate
    raise [Not_found] if no element is found *)

val find_exc : ('a -> bool) -> exn -> 'a t -> 'a
(** Same as find but takes an exception to be raised when
    no element is found as additional parameter *)

val exists : ('a -> bool) -> 'a t -> bool
(** Return [true] if an element matches the specified
    predicate *)

val for_all : ('a -> bool) -> 'a t -> bool
(** Return [true] if all elements match the specified
    predicate *)

val map : ('a -> 'b) -> 'a t -> 'b t
(** Apply a function to all elements
    and return the ref list constructed with
    the function returned values *)

val transform : ('a -> 'a) -> 'a t -> unit
(** transform all elements in the ref list
    using a function. *)

val map_list : ('a -> 'b) -> 'a t -> 'b list
(** Apply a function to all elements
    and return the list constructed with
    the function returned values *)

val sort : ?cmp:('a -> 'a -> int) -> 'a t -> unit
(** Sort elements using the specified comparator
    or compare as default comparator *)

val filter : ('a -> bool) -> 'a t -> unit
(** Remove all elements that do not match the
    specified predicate *)

val remove : 'a t -> 'a -> unit
(** Remove an element from the ref list
    raise [Not_found] if the element is not found *)

val remove_if : ('a -> bool) -> 'a t -> unit
(** Remove the first element matching the
    specified predicate
    raise [Not_found] if no element has been removed *)

val remove_all : 'a t -> 'a -> unit
(** Remove all elements equal to the specified
    element from the ref list *)



(** Functions that operate on the [i]th element of a list.

    While it is sometimes necessary to perform these
    operations on lists (hence their inclusion here), the
    functions were moved to an inner module to prevent
    their overuse: all functions work in O(n) time. You
	might prefer to use [Array] or [DynArray] for constant
	time indexed element access.
*)
module Index : sig

	val index_of : 'a t -> 'a -> int
	(** Return the index (position : 0 starting) of an element in
	    a ref list, using ( = ) for testing element equality
	    raise [Not_found] if no element was found *)

	val index : ('a -> bool) -> 'a t -> int
	(** Return the index (position : 0 starting) of an element in
	    a ref list, using the specified comparator
	    raise [Not_found] if no element was found *)

	val at_index : 'a t -> int -> 'a
	(** Return the element of ref list at the specified index
	    raise [Invalid_index] if the index is outside [0 ; length-1] *)

	val set : 'a t -> int -> 'a -> unit
	(** Change the element at the specified index
	    raise [Invalid_index] if the index is outside [0 ; length-1] *)

	val remove_at : 'a t -> int -> unit
	(** Remove the element at the specified index
	    raise [Invalid_index] if the index is outside [0 ; length-1] *)

end
