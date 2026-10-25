(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2016 OCamlPro SAS                                    *)
(*   Copyright 2014--2016 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-9-30-40-41-42"]

include Identifiable.S

val create
   : ?current_compilation_unit:Compilation_unit.t
  -> Internal_variable_names.t
  -> t

val create_with_same_name_as_ident : Ident.t -> t

val create_from_variable
  : ?current_compilation_unit:Compilation_unit.t
  -> Variable.t
  -> t

val rename
   : ?current_compilation_unit:Compilation_unit.t
  -> t
  -> t

val in_compilation_unit : t -> Compilation_unit.t -> bool

val name : t -> string

val unique_name : t -> string

val print_list : Format.formatter -> t list -> unit
val print_opt : Format.formatter -> t option -> unit

val output_full : out_channel -> t -> unit
