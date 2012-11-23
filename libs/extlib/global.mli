(*
 * Global - Mutable global variable
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

(** Mutable global variable.
 
	Often in OCaml you want to have a global variable, which is mutable 
    and uninitialized when declared. You can use a ['a option ref] but
	this is not very convenient. The Global module provides functions
	to easily create and manipulate such variables.
*)

type 'a t
(** Abstract type of a global *)

exception Global_not_initialized of string
(** Raised when a global variable is accessed without first having been
 assigned a value. The parameter contains the name of the global. *)

val empty : string -> 'a t
(** Returns an new named empty global. The name of the global can be any
 string. It identifies the global and makes debugging easier. *)

val name : 'a t -> string
(** Retrieve the name of a global. *)

val set : 'a t -> 'a -> unit
(** Set the global value contents. *)

val get : 'a t -> 'a
(** Get the global value contents - raise Global_not_initialized if not
 defined. *)

val undef : 'a t -> unit 
(** Reset the global value contents to undefined. *)

val isdef : 'a t -> bool 
  (** Return [true] if the global value has been set. *)

val opt : 'a t -> 'a option 
  (** Return [None] if the global is undefined, else [Some v] where v is the
  current global value contents. *)
