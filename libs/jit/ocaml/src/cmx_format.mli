(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Gallium, INRIA Rocquencourt           *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2010 Institut National de Recherche en Informatique et     *)
(*     en Automatique                                                     *)
(*   Copyright 2013--2016 OCamlPro SAS                                    *)
(*   Copyright 2014--2016 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Format of .cmx and .cmxa files *)

open Misc

(* Each .o file has a matching .cmx file that provides the following infos
   on the compilation unit:
     - list of other units imported, with MD5s of their .cmx files
     - approximation of the structure implemented
       (includes descriptions of known functions: arity and direct entry
        points)
     - list of currying functions and application functions needed
   The .cmx file contains these infos (as an externed record) plus a MD5
   of these infos *)

type export_info =
  | Clambda of Clambda.value_approximation
  | Flambda of Export_info.t

type unit_infos =
  { mutable ui_name: modname;             (* Name of unit implemented *)
    mutable ui_symbol: string;            (* Prefix for symbols *)
    mutable ui_defines: string list;      (* Unit and sub-units implemented *)
    mutable ui_imports_cmi: crcs;         (* Interfaces imported *)
    mutable ui_imports_cmx: crcs;         (* Infos imported *)
    mutable ui_curry_fun: int list;       (* Currying functions needed *)
    mutable ui_apply_fun: int list;       (* Apply functions needed *)
    mutable ui_send_fun: int list;        (* Send functions needed *)
    mutable ui_export_info: export_info;
    mutable ui_force_link: bool;          (* Always linked *)
    mutable ui_for_pack: string option }  (* Part of a pack *)

(* Each .a library has a matching .cmxa file that provides the following
   infos on the library: *)

type library_infos =
  { lib_units: (unit_infos * Digest.t) list;  (* List of unit infos w/ MD5s *)
    (* In the following fields the lists are reversed with respect to
       how they end up being used on the command line. *)
    lib_ccobjs: string list;            (* C object files needed *)
    lib_ccopts: string list }           (* Extra opts to C compiler *)
