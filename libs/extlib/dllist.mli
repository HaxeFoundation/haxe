(*
 * Dllist- a mutable, circular, doubly linked list library
 * Copyright (C) 2004 Brian Hurt, Jesse Guardiani
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

(** A mutable, imperative, circular, doubly linked list library

    This module implements a doubly linked list in a mutable or imperitive
    style (changes to the list are visible to all copies of the list).
*)


type 'a node_t (* abstract *)

exception Empty

(** {6 node functions } *)

(** Creates a node.  This is an O(1) operation. *)
val create : 'a -> 'a node_t

(** Copy the list attached to the given node and return the copy of the given
    node.  This is an O(N) operation.
*)
val copy : 'a node_t -> 'a node_t

(** Returns the length of the list.  This is an O(N) operation. *)
val length : 'a node_t -> int

(** List reversal.  This is an O(N) operation.
*)
val rev : 'a node_t -> unit

(** [add n a] Creates a new node containing data [a] and inserts it into
    the list after node [n].  This is an O(1) operation.
*)
val add : 'a node_t -> 'a -> unit

(** [append n a] Creates a new node containing data [a] and inserts it into
    the list after node [n]. Returns new node.  This is an O(1) operation.
*)
val append : 'a node_t -> 'a -> 'a node_t

(** [prepend n a] Creates a new node containing data [a] and inserts it into
    the list before node [n]. Returns new node.  This is an O(1) operation.
*)
val prepend : 'a node_t -> 'a -> 'a node_t

(** [promote n] Swaps [n] with [next n].  This is an O(1) operation.
*)
val promote : 'a node_t -> unit

(** [demote n] Swaps [n] with [prev n].  This is an O(1) operation.
*)
val demote : 'a node_t -> unit

(** Remove node from the list no matter where it is.  This is an O(1) operation.
*)
val remove : 'a node_t -> unit

(** Remove node from the list no matter where it is. Return next node.  This is
    an O(1) operation.
*)
val drop : 'a node_t -> 'a node_t

(** Remove node from the list no matter where it is. Return previous node.  This
    is an O(1) operation.
*)
val rev_drop : 'a node_t -> 'a node_t

(** [splice n1 n2] Connects [n1] and [n2] so that
    [next n1 == n2 && prev n2 == n1]. This can be used to connect two discrete
    lists, or, if used on two nodes within the same list, it can be used to
    separate the nodes between [n1] and [n2] from the rest of the list. In this
    case, those nodes become a discrete list by themselves.  This is an O(1)
    operation.
*)   
val splice : 'a node_t -> 'a node_t -> unit

(** Given a node, get the data associated with that node.  This is an
    O(1) operation.
*)
val get : 'a node_t -> 'a

(** Given a node, set the data associated with that node.  This is an O(1)
    operation.
*)
val set : 'a node_t -> 'a -> unit

(** Given a node, get the next element in the list after the node.  

    The list is circular, so the last node of the list returns the first 
    node of the list as it's next node.
    
    This is an O(1) operation.
*)
val next : 'a node_t -> 'a node_t

(** Given a node, get the previous element in the list before the node.
 
    The list is circular, so the first node of the list returns the
    last element of the list as it's previous node.

    This is an O(1) operation.
*)
val prev : 'a node_t -> 'a node_t

(** [skip n i] Return the node that is [i] nodes after node [n] in the list.
    If [i] is negative then return the node that is [i] nodes before node [n]
    in the list.  This is an O(N) operation.
*)
val skip : 'a node_t -> int -> 'a node_t

(** [iter f n] Apply [f] to every element in the list, starting at [n].  This
    is an O(N) operation.
*)
val iter : ('a -> unit) -> 'a node_t -> unit

(** Accumulate a value over the entire list.  
    This works like List.fold_left. This is an O(N) operation.
*)
val fold_left : ('a -> 'b -> 'a) -> 'a -> 'b node_t -> 'a

(** Accumulate a value over the entire list.
    This works like List.fold_right, but since the list is bidirectional,
    it doesn't suffer the performance problems of List.fold_right.
    This is an O(N) operation.
*)
val fold_right : ('a -> 'b -> 'b) -> 'a node_t -> 'b -> 'b

(** Allocate a new list, with entirely new nodes, whose values are
    the transforms of the values of the original list.  Note that this
    does not modify the given list.  This is an O(N) operation.
*)
val map : ('a -> 'b) -> 'a node_t -> 'b node_t


(** {6 list conversion } *)

(** Converts a dllist to a normal list.  This is an O(N) operation. *)
val to_list : 'a node_t -> 'a list

(** Converts from a normal list to a Dllist and returns the first node. Raises
    [Empty] if given list is empty.  This is an O(N) operation.
*)
val of_list : 'a list -> 'a node_t


(** {6 enums } *)

(** Create an enum of the list.
    Note that modifying the list while the enum exists will have undefined
    effects.  This is an O(1) operation.
*)
val enum : 'a node_t -> 'a Enum.t

(** Create a reverse enum of the list.
    Note that modifying the list while the enum exists will have undefined
    effects.  This is an O(1) operation.
*)
val rev_enum : 'a node_t -> 'a Enum.t

(** Create a dllist from an enum.
    This consumes the enum, and allocates a whole new dllist. Raises
    [Empty] if given enum is empty.  This is an O(N) operation.
*)
val of_enum : 'a Enum.t -> 'a node_t
