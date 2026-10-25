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

open! Int_replace_polymorphic_compare
open Lexing
open Location

module Scoped_location = struct
  type scope_item =
    | Sc_anonymous_function
    | Sc_value_definition
    | Sc_module_definition
    | Sc_class_definition
    | Sc_method_definition

  type scopes =
    | Empty
    | Cons of {item: scope_item; str: string; str_fun: string}

  let str_fun = function
    | Empty -> "(fun)"
    | Cons r -> r.str_fun

  let cons item str =
    Cons {item; str; str_fun = str ^ ".(fun)"}

  let empty_scopes = Empty

  let add_parens_if_symbolic = function
    | "" -> ""
    | s ->
       match s.[0] with
       | 'a'..'z' | 'A'..'Z' | '_' | '0'..'9' -> s
       | _ -> "(" ^ s ^ ")"

  let dot ?(sep = ".") scopes s =
    let s = add_parens_if_symbolic s in
    match scopes with
    | Empty -> s
    | Cons {str; _} -> str ^ sep ^ s

  let enter_anonymous_function ~scopes =
    let str = str_fun scopes in
    Cons {item = Sc_anonymous_function; str; str_fun = str}

  let enter_value_definition ~scopes id =
    cons Sc_value_definition (dot scopes (Ident.name id))

  let enter_module_definition ~scopes id =
    cons Sc_module_definition (dot scopes (Ident.name id))

  let enter_class_definition ~scopes id =
    cons Sc_class_definition (dot scopes (Ident.name id))

  let enter_method_definition ~scopes (s : Asttypes.label) =
    let str =
      match scopes with
      | Cons {item = Sc_class_definition; _} -> dot ~sep:"#" scopes s
      | _ -> dot scopes s
    in
    cons Sc_method_definition str

  let string_of_scopes = function
    | Empty -> "<unknown>"
    | Cons {str; _} -> str

  type t =
    | Loc_unknown
    | Loc_known of
        { loc : Location.t;
          scopes : scopes; }

  let of_location ~scopes loc =
    if Location.is_none loc then
      Loc_unknown
    else
      Loc_known { loc; scopes }

  let to_location = function
    | Loc_unknown -> Location.none
    | Loc_known { loc; _ } -> loc

  let string_of_scoped_location = function
    | Loc_unknown -> "??"
    | Loc_known { loc = _; scopes } -> string_of_scopes scopes
end

type item = {
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
type alloc_dbginfo = alloc_dbginfo_item list

let none = []

let is_none = function
  | [] -> true
  | _ :: _ -> false

let to_string dbg =
  match dbg with
  | [] -> ""
  | ds ->
    let items =
      List.map
        (fun d ->
           Printf.sprintf "%s:%d,%d-%d"
             d.dinfo_file d.dinfo_line d.dinfo_char_start d.dinfo_char_end)
        ds
    in
    "{" ^ String.concat ";" items ^ "}"

let item_from_location ~scopes loc =
  let valid_endpos =
    String.equal loc.loc_end.pos_fname loc.loc_start.pos_fname in
  { dinfo_file = loc.loc_start.pos_fname;
    dinfo_line = loc.loc_start.pos_lnum;
    dinfo_char_start = loc.loc_start.pos_cnum - loc.loc_start.pos_bol;
    dinfo_char_end =
      if valid_endpos
      then loc.loc_end.pos_cnum - loc.loc_start.pos_bol
      else loc.loc_start.pos_cnum - loc.loc_start.pos_bol;
    dinfo_start_bol = loc.loc_start.pos_bol;
    dinfo_end_bol =
      if valid_endpos then loc.loc_end.pos_bol
      else loc.loc_start.pos_bol;
    dinfo_end_line =
      if valid_endpos then loc.loc_end.pos_lnum
      else loc.loc_start.pos_lnum;
    dinfo_scopes = scopes
  }

let from_location = function
  | Scoped_location.Loc_unknown -> []
  | Scoped_location.Loc_known {scopes; loc} ->
    assert (not (Location.is_none loc));
    [item_from_location ~scopes loc]

let to_location = function
  | [] -> Location.none
  | d :: _ ->
    let loc_start =
      { pos_fname = d.dinfo_file;
        pos_lnum = d.dinfo_line;
        pos_bol = d.dinfo_start_bol;
        pos_cnum = d.dinfo_start_bol + d.dinfo_char_start;
      } in
    let loc_end =
      { pos_fname = d.dinfo_file;
        pos_lnum = d.dinfo_end_line;
        pos_bol = d.dinfo_end_bol;
        pos_cnum = d.dinfo_start_bol + d.dinfo_char_end;
      } in
    { loc_ghost = false; loc_start; loc_end; }

let inline dbg1 dbg2 =
  dbg1 @ dbg2

(* CR-someday afrisch: FWIW, the current compare function does not seem very
   good, since it reverses the two lists. I don't know how long the lists are,
   nor if the specific currently implemented ordering is useful in other
   contexts, but if one wants to use Map, a more efficient comparison should
   be considered. *)
let compare dbg1 dbg2 =
  let rec loop ds1 ds2 =
    match ds1, ds2 with
    | [], [] -> 0
    | _ :: _, [] -> 1
    | [], _ :: _ -> -1
    | d1 :: ds1, d2 :: ds2 ->
      let c = String.compare d1.dinfo_file d2.dinfo_file in
      if c <> 0 then c else
      let c = compare d1.dinfo_line d2.dinfo_line in
      if c <> 0 then c else
      let c = compare d1.dinfo_char_end d2.dinfo_char_end in
      if c <> 0 then c else
      let c = compare d1.dinfo_char_start d2.dinfo_char_start in
      if c <> 0 then c else
      let c = compare d1.dinfo_start_bol d2.dinfo_start_bol in
      if c <> 0 then c else
      let c = compare d1.dinfo_end_bol d2.dinfo_end_bol in
      if c <> 0 then c else
      let c = compare d1.dinfo_end_line d2.dinfo_end_line in
      if c <> 0 then c else
      loop ds1 ds2
  in
  loop (List.rev dbg1) (List.rev dbg2)

let hash t =
  List.fold_left (fun hash item -> Hashtbl.hash (hash, item)) 0 t

let rec print_compact ppf t =
  let print_item item =
    Format.fprintf ppf "%a:%i"
      Location.print_filename item.dinfo_file
      item.dinfo_line;
    if item.dinfo_char_start >= 0 then begin
      Format.fprintf ppf ",%i--%i" item.dinfo_char_start item.dinfo_char_end
    end
  in
  match t with
  | [] -> ()
  | [item] -> print_item item
  | item::t ->
    print_item item;
    Format.fprintf ppf ";";
    print_compact ppf t
