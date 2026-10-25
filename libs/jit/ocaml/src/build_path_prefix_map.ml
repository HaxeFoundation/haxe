(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*            Gabriel Scherer, projet Parsifal, INRIA Saclay              *)
(*                                                                        *)
(*   Copyright 2017 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

type path = string
type path_prefix = string
type error_message = string

let errorf fmt = Printf.ksprintf (fun err -> Error err) fmt

let encode_prefix str =
  let buf = Buffer.create (String.length str) in
  let push_char = function
    | '%' -> Buffer.add_string buf "%#"
    | '=' -> Buffer.add_string buf "%+"
    | ':' -> Buffer.add_string buf "%."
    | c -> Buffer.add_char buf c
  in
  String.iter push_char str;
  Buffer.contents buf

let decode_prefix str =
  let buf = Buffer.create (String.length str) in
  let rec loop i =
    if i >= String.length str
    then Ok (Buffer.contents buf)
    else match str.[i] with
      | ('=' | ':') as c ->
        errorf "invalid character '%c' in key or value" c
      | '%' ->
        let push c = Buffer.add_char buf c; loop (i + 2) in
        if i + 1 = String.length str then
          errorf "invalid encoded string %S (trailing '%%')" str
        else begin match str.[i + 1] with
            | '#' -> push '%'
            | '+' -> push '='
            | '.' -> push ':'
            | c -> errorf "invalid %%-escaped character '%c'" c
        end
      | c ->
        Buffer.add_char buf c;
        loop (i + 1)
  in loop 0

type pair = { target: path_prefix; source : path_prefix }

let encode_pair { target; source } =
  String.concat "=" [encode_prefix target; encode_prefix source]

let decode_pair str =
  match String.index str '=' with
  | exception Not_found ->
    errorf "invalid key/value pair %S, no '=' separator" str
  | equal_pos ->
    let encoded_target = String.sub str 0 equal_pos in
    let encoded_source =
      String.sub str (equal_pos + 1) (String.length str - equal_pos - 1) in
    match decode_prefix encoded_target, decode_prefix encoded_source with
    | Ok target, Ok source -> Ok { target; source }
    | ((Error _ as err), _) | (_, (Error _ as err)) -> err

type map = pair option list

let encode_map map =
  let encode_elem = function
    | None -> ""
    | Some pair -> encode_pair pair
  in
  List.map encode_elem map
  |> String.concat ":"

let decode_map str =
  let exception Shortcut of error_message in
  let decode_or_empty = function
    | "" -> None
    | pair ->
      begin match decode_pair pair with
        | Ok str -> Some str
        | Error err -> raise (Shortcut err)
      end
  in
  let pairs = String.split_on_char ':' str in
  match List.map decode_or_empty pairs with
  | exception (Shortcut err) -> Error err
  | map -> Ok map

let make_target path : pair option -> path option = function
  | None -> None
  | Some { target; source } ->
    let is_prefix =
      String.length source <= String.length path
        && String.equal source (String.sub path 0 (String.length source)) in
    if is_prefix then
      Some (target ^ (String.sub path (String.length source)
                       (String.length path - String.length source)))
    else None

let rewrite_first prefix_map path =
  List.find_map (make_target path) (List.rev prefix_map)

let rewrite_all prefix_map path =
  List.filter_map (make_target path) (List.rev prefix_map)

let rewrite prefix_map path =
  match rewrite_first prefix_map path with
  | None -> path
  | Some path -> path
