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

[@@@ocaml.warning "+a-4-30-40-41-42"]

module S = Misc.Stdlib.String

type is_global = Global | Local

type constant =
  | Const_closure of is_global * Clambda.ufunction list * Clambda.uconstant list
  | Const_table of is_global * Cmm.data_item list

type t = {
  mutable constants : constant S.Map.t;
  mutable data_items : Cmm.data_item list list;
  structured_constants : (string,  Clambda.ustructured_constant) Hashtbl.t;
  functions : Clambda.ufunction Queue.t;
}

let empty = {
  constants = S.Map.empty;
  data_items = [];
  functions = Queue.create ();
  structured_constants = Hashtbl.create 16;
}

let state = empty

let add_constant sym cst =
  state.constants <- S.Map.add sym cst state.constants

let add_data_items items =
  state.data_items <- items :: state.data_items

let add_function func =
  Queue.add func state.functions

let get_and_clear_constants () =
  let constants = state.constants in
  state.constants <- S.Map.empty;
  constants

let get_and_clear_data_items () =
  let data_items = List.concat (List.rev state.data_items) in
  state.data_items <- [];
  data_items

let next_function () =
  match Queue.take state.functions with
  | exception Queue.Empty -> None
  | func -> Some func

let no_more_functions () =
  Queue.is_empty state.functions

let set_structured_constants l =
  Hashtbl.clear state.structured_constants;
  List.iter
    (fun (c : Clambda.preallocated_constant) ->
       Hashtbl.add state.structured_constants c.symbol c.definition
    )
    l

let add_structured_constant sym cst =
  Hashtbl.replace state.structured_constants sym cst

let get_structured_constant s =
  Hashtbl.find_opt state.structured_constants s

let structured_constant_of_sym s =
  match Compilenv.structured_constant_of_symbol s with
  | None -> get_structured_constant s
  | Some _ as r -> r
