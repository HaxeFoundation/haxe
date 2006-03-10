(*
 * Std - Additional functions
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

(** Additional functions. *)

val input_lines : in_channel -> string Enum.t
(** Returns an enumeration over lines of an input channel, as read by the
 [input_line] function. *)

val input_chars : in_channel -> char Enum.t
(** Returns an enumeration over characters of an input channel. *)

val input_list : in_channel -> string list
(** Returns the list of lines read from an input channel. *)

val input_all : in_channel -> string
(** Return the whole contents of an input channel as a single
 string. *)

val print_bool : bool -> unit
(** Print a boolean to stdout. *)

val prerr_bool : bool -> unit
(** Print a boolean to stderr. *)

val input_file : ?bin:bool -> string -> string
(** returns the data of a given filename. *)

val output_file : filename:string -> text:string -> unit
(** creates a filename, write text into it and close it. *)

val string_of_char : char -> string
(** creates a string from a char. *)

external identity : 'a -> 'a = "%identity"
(** the identity function. *)

val unique : unit -> int
(** returns an unique identifier every time it is called. *)

val dump : 'a -> string
(** represent a runtime value as a string. Since types are lost at compile
	time, the representation might not match your type. For example, None
	will be printed 0 since they share the same runtime representation. *)

val print : 'a -> unit
(** print the representation of a runtime value on stdout.
	See remarks for [dump]. *)

val finally : (unit -> unit) -> ('a -> 'b) -> 'a -> 'b 
(** finally [fend f x] calls [f x] and then [fend()] even if [f x] raised
	an exception. *)
