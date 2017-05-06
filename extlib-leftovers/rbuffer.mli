(**************************************************************************)
(*                                                                        *)
(*  Copyright (C) Jean-Christophe Filliatre                               *)
(*                                                                        *)
(*  This software is free software; you can redistribute it and/or        *)
(*  modify it under the terms of the GNU Library General Public           *)
(*  License version 2.1, with the special exception on linking            *)
(*  described in file LICENSE.                                            *)
(*                                                                        *)
(*  This software is distributed in the hope that it will be useful,      *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                  *)
(*                                                                        *)
(**************************************************************************)

(* Ropes-based implementation of Buffer *)

type t

val create : int -> t
val reset : t -> unit
val clear : t -> unit
val length : t -> int

val unsafe_contents : t -> string

val sub : t -> int -> int -> string
val nth : t -> int -> char


val add_char : t -> char -> unit

val add_substring : t -> string -> int -> int -> unit
val add_string : t -> string -> unit
val add_buffer : t -> t -> unit

val add_channel : t -> in_channel -> int -> unit

val output_buffer : out_channel -> t -> unit
