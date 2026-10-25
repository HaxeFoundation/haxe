(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1997 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Auxiliary type for reporting syntax errors

  {b Warning:} this module is unstable and part of
  {{!Compiler_libs}compiler-libs}.

*)

type invalid_package_type =
  | Parameterized_types
  | Constrained_types
  | Private_types
  | Not_with_type
  | Neither_identifier_nor_with_type

type error =
    Unclosed of Location.t * string * Location.t * string
  | Expecting of Location.t * string
  | Not_expecting of Location.t * string
  | Applicative_path of Location.t
  | Variable_in_scope of Location.t * string
  | Other of Location.t
  | Ill_formed_ast of Location.t * string
  | Invalid_package_type of Location.t * invalid_package_type
  | Removed_string_set of Location.t

exception Error of error
exception Escape_error

val location_of_error: error -> Location.t
val ill_formed_ast: Location.t -> string -> 'a
