(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                  Jeremie Dimino, Jane Street Europe                    *)
(*                                                                        *)
(*   Copyright 2015 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open Asttypes
open Parsetree

module Style = Misc.Style

type error =
  | Multiple_attributes of string
  | No_payload_expected of string

exception Error of Location.t * error

let get_no_payload_attribute nm attrs =
  let actions = [(nm, Builtin_attributes.Return)] in
  match Builtin_attributes.select_attributes actions attrs with
  | [] -> None
  | [ {attr_name = name; attr_payload = PStr []; attr_loc = _} ] -> Some name
  | [ {attr_name = name; _} ] ->
    raise (Error (name.loc, No_payload_expected name.txt))
  | _ :: {attr_name = name; _} :: _ ->
    raise (Error (name.loc, Multiple_attributes name.txt))

let has_no_payload_attribute alt_names attrs =
  match get_no_payload_attribute alt_names attrs with
  | None   -> false
  | Some _ -> true

open Format_doc

let report_error_doc ppf = function
  | Multiple_attributes name ->
    fprintf ppf "Too many %a attributes" Style.inline_code name
  | No_payload_expected name ->
    fprintf ppf "Attribute %a does not accept a payload" Style.inline_code name

let () =
  Location.register_error_of_exn
    (function
      | Error (loc, err) ->
        Some (Location.error_of_printer ~loc report_error_doc err)
      | _ ->
        None
    )

let report_error = Format_doc.compat report_error_doc
