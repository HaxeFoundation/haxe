(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Gallium, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 2006 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

module Scoped_location : sig
  type scopes
  val string_of_scopes : scopes -> string

  val empty_scopes : scopes
  val enter_anonymous_function : scopes:scopes -> scopes
  val enter_value_definition : scopes:scopes -> Ident.t -> scopes
  val enter_module_definition : scopes:scopes -> Ident.t -> scopes
  val enter_class_definition : scopes:scopes -> Ident.t -> scopes
  val enter_method_definition : scopes:scopes -> Asttypes.label -> scopes

  type t =
    | Loc_unknown
    | Loc_known of
        { loc : Location.t;
          scopes : scopes; }

  val of_location : scopes:scopes -> Location.t -> t
  val to_location : t -> Location.t
  val string_of_scoped_location : t -> string
end

type item = private {
  dinfo_file: string;
  dinfo_line: int;
  dinfo_char_start: int;
  dinfo_char_end: int;
  dinfo_start_bol: int;
  dinfo_end_bol: int;
  dinfo_end_line: int;
  dinfo_scopes: Scoped_location.scopes;
}

type t = item list

type alloc_dbginfo_item =
  { alloc_words : int;
    alloc_dbg : t }
(** Due to Comballoc, a single Ialloc instruction may combine several
    unrelated allocations. Their Debuginfo.t (which may differ) are stored
    as a list of alloc_dbginfo. This list is in order of increasing memory
    address, which is the reverse of the original allocation order. Later
    allocations are consed to the front of this list by Comballoc. *)

type alloc_dbginfo = alloc_dbginfo_item list

val none : t

val is_none : t -> bool

val to_string : t -> string

val from_location : Scoped_location.t -> t

val to_location : t -> Location.t

val inline : t -> t -> t

val compare : t -> t -> int

val hash : t -> int

val print_compact : Format.formatter -> t -> unit
