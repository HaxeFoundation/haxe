(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                    Marcell Fischbach, University of Siegen             *)
(*                     Benedikt Meurer, University of Siegen              *)
(*                                                                        *)
(*   Copyright 2011 Lehrstuhl für Compilerbau und Softwareanalyse,        *)
(*     Universität Siegen.                                                *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Live intervals for the linear scan register allocator. *)

type range =
  {
    mutable rbegin: int;
    mutable rend: int;
  }

type t =
  {
    mutable reg: Reg.t;
    mutable ibegin: int;
    mutable iend: int;
    mutable ranges: range list;
  }

type result =
  {
    intervals : t list;
    fixed_intervals : t list;
  }

val overlap: t -> t -> bool
val is_live: t -> int -> bool
val remove_expired_ranges: t -> int -> unit
val build_intervals: Mach.fundecl -> result
