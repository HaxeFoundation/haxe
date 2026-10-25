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

external zstd_initialize: unit -> bool = "caml_zstd_initialize"

let compression_supported = zstd_initialize ()

type [@warning "-unused-constructor"] extern_flags =
    No_sharing                          (** Don't preserve sharing *)
  | Closures                            (** Send function closures *)
  | Compat_32                           (** Ensure 32-bit compatibility *)
  | Compression                         (** Optional compression *)

external to_channel: out_channel -> 'a -> extern_flags list -> unit
                   = "caml_output_value"

let output_value ch v = to_channel ch v [Compression]

let input_value = Stdlib.input_value
