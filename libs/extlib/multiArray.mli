(*
 * MultiArray - Resizeable Ocaml big arrays
 * Copyright (C) 201 Nicolas Cannasse
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

(** Dynamic Big arrays.

   A dynamic array is equivalent to a OCaml array that will resize itself
   when elements are added or removed. MultiArray is different from DynArray
   since it allows more than 4 Millions elements on 32 bits systems.

   A MultiArray of size <= Sys.max_array_length will use a single indirection
   internal representation. If the size exceeds Sys.max_array_length, e.g. by
   adding an additional element, the internal representation is promoted to use
   double indirection. This allows for bigger arrays, but it also slower.
*)

type 'a t

exception Invalid_arg of int * string * string
(** When an operation on an array fails, [Invalid_arg] is raised. The
	integer is the value that made the operation fail, the first string
	contains the function name that has been called and the second string
	contains the parameter name that made the operation fail.
*)

(** {6 MultiArray creation} *)

val create : unit -> 'a t
(** [create()] returns a new empty dynamic array. *)

val make : int -> 'a -> 'a t
(** [make count value] returns an array with some memory already allocated and
	[count] elements initialized to [value]. *)

val init : int -> (int -> 'a) -> 'a t
(** [init n f] returns an array of [n] elements filled with values
	returned by [f 0 , f 1, ... f (n-1)]. *)

(** {6 MultiArray manipulation functions} *)

val empty : 'a t -> bool
(** Return true if the number of elements in the array is 0. *)

val length : 'a t -> int
(** Return the number of elements in the array. *)

val get : 'a t -> int -> 'a
(** [get darr idx] gets the element in [darr] at index [idx]. If [darr] has
	[len] elements in it, then the valid indexes range from [0] to [len-1]. *)

val set : 'a t -> int -> 'a -> unit
(** [set darr idx v] sets the element of [darr] at index [idx] to value
	[v].  The previous value is overwritten. *)

val add : 'a t -> 'a -> unit
(** [add darr v] appends [v] onto [darr].  [v] becomes the new
	last element of [darr]. If required, the size of the internal representation
	is doubled. If this would exceed Sys.max_array_length, the internal
	representation is automatically changed to double indirection and the
	current contents are copied over. *)

val clear : 'a t -> unit
(** remove all elements from the array and resize it to 0. *)

(** {6 MultiArray copy and conversion} *)

val of_array : 'a array -> 'a t
(** [of_array arr] returns an array with the elements of [arr] in it
	in order. *)

val of_list : 'a list -> 'a t
(** [of_list lst] returns a dynamic array with the elements of [lst] in
	it in order. *)

(** {6 MultiArray functional support} *)

val iter : ('a -> unit) -> 'a t -> unit
(** [iter f darr] calls the function [f] on every element of [darr].  It
	is equivalent to [for i = 0 to length darr - 1 do f (get darr i) done;] *)

val iteri : (int -> 'a -> unit) -> 'a t -> unit
(** [iter f darr] calls the function [f] on every element of [darr].  It
	is equivalent to [for i = 0 to length darr - 1 do f i (get darr i) done;]
	*)

val map : ('a -> 'b) -> 'a t -> 'b t
(** [map f darr] applies the function [f] to every element of [darr]
	and creates a dynamic array from the results - similar to [List.map] or
	[Array.map]. *)

val mapi : (int -> 'a -> 'b) -> 'a t -> 'b t
(** [mapi f darr] applies the function [f] to every element of [darr]
	and creates a dynamic array from the results - similar to [List.mapi] or
	[Array.mapi]. *)

val fold_left : ('b -> 'a -> 'b) -> 'b -> 'a t -> 'b
(** [fold_left f x darr] computes
	[f ( ... ( f ( f (get darr 0) x) (get darr 1) ) ... ) (get darr n-1)],
	similar to [Array.fold_left] or [List.fold_left]. *)