(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                     Mark Shinwell, Jane Street Europe                  *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*   Copyright 2019 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Mutable state used by [Cmmgen]. *)

[@@@ocaml.warning "+a-4-30-40-41-42"]

type is_global = Global | Local

type constant =
  | Const_closure of is_global * Clambda.ufunction list * Clambda.uconstant list
  | Const_table of is_global * Cmm.data_item list

val add_constant : Misc.Stdlib.String.t -> constant -> unit

val add_data_items : Cmm.data_item list -> unit

val add_function : Clambda.ufunction -> unit

val get_and_clear_constants : unit -> constant Misc.Stdlib.String.Map.t

val get_and_clear_data_items : unit -> Cmm.data_item list

val next_function : unit -> Clambda.ufunction option

val no_more_functions : unit -> bool

val set_structured_constants : Clambda.preallocated_constant list -> unit

val add_structured_constant : string -> Clambda.ustructured_constant -> unit

(* Also looks up using Compilenv.structured_constant_of_symbol *)
val structured_constant_of_sym : string -> Clambda.ustructured_constant option
