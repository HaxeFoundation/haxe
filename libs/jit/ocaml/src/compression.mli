(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*        Xavier Leroy, Collège de France and Inria project Cambium       *)
(*                                                                        *)
(*   Copyright 2023 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

val output_value : out_channel -> 'a -> unit
(** [Compression.output_value chan v] writes the representation
    of [v] on channel [chan].
    If compression is supported, the marshaled data
    representing value [v] is compressed before being written to
    channel [chan].
    If compression is not supported, this function behaves like
    {!Stdlib.output_value}. *)

val input_value : in_channel -> 'a
(** [Compression.input_value chan] reads from channel [chan] the
    byte representation of a structured value, as produced by
    [Compression.output_value], and reconstructs and
    returns the corresponding value.
    If compression is not supported, this function behaves like
    {!Stdlib.input_value}. *)

val compression_supported : bool
(** Reports whether compression is supported. *)
