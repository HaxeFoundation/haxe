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

   MultiArray is slower since it requires an additional level of indirection.
*)

type 'a t

exception Invalid_arg of int * string * string

val create : unit -> 'a t
val make : int -> 'a -> 'a t
val init : int -> (int -> 'a) -> 'a t

val empty : 'a t -> bool
val length : 'a t -> int

val get : 'a t -> int -> 'a
val set : 'a t -> int -> 'a -> unit
val add : 'a t -> 'a -> unit
val clear : 'a t -> unit

val of_array : 'a array -> 'a t
val of_list : 'a list -> 'a t

val iter : ('a -> unit) -> 'a t -> unit
val iteri : (int -> 'a -> unit) -> 'a t -> unit

val map : ('a -> 'b) -> 'a t -> 'b t
val mapi : (int -> 'a -> 'b) -> 'a t -> 'b t

val fold_left : ('b -> 'a -> 'b) -> 'b -> 'a t -> 'b