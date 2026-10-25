(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 2002 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Consistency tables: for checking consistency of module CRCs

  {b Warning:} this module is unstable and part of
  {{!Compiler_libs}compiler-libs}.

*)

open Misc

module Make (Module_name : sig
  type t
  module Set : Set.S with type elt = t
  module Map : Map.S with type key = t
  module Tbl : Hashtbl.S with type key = t
  val compare : t -> t -> int
end) : sig
  type t

  val create: unit -> t

  val clear: t -> unit

  val check: t -> Module_name.t -> Digest.t -> filepath -> unit
        (* [check tbl name crc source]
             checks consistency of ([name], [crc]) with infos previously
             stored in [tbl].  If no CRC was previously associated with
             [name], record ([name], [crc]) in [tbl].
             [source] is the name of the file from which the information
             comes from.  This is used for error reporting. *)

  val check_noadd: t -> Module_name.t -> Digest.t -> filepath -> unit
        (* Same as [check], but raise [Not_available] if no CRC was previously
             associated with [name]. *)

  val source: t -> Module_name.t -> filepath
        (* [source tbl name] returns the file name associated with [name]
           if the latter has an associated CRC in [tbl].
           Raise [Not_found] otherwise. *)

  val extract: Module_name.t list -> t -> (Module_name.t * Digest.t option) list
        (* [extract tbl names] returns an associative list mapping each string
           in [names] to the CRC associated with it in [tbl]. If no CRC is
           associated with a name then it is mapped to [None]. *)

  val extract_map : Module_name.Set.t -> t -> Digest.t option Module_name.Map.t
        (* Like [extract] but with a more sophisticated type. *)

  val filter: (Module_name.t -> bool) -> t -> unit
        (* [filter pred tbl] removes from [tbl] table all (name, CRC) pairs
           such that [pred name] is [false]. *)

  exception Inconsistency of {
    unit_name : Module_name.t;
    inconsistent_source : string;
    original_source : string;
  }
  (* Raised by [check] when a CRC mismatch is detected. *)

  exception Not_available of Module_name.t
        (* Raised by [check_noadd] when a name doesn't have an associated
           CRC. *)
end
