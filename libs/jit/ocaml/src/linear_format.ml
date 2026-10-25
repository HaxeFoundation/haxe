(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                    Greta Yorsh, Jane Street Europe                     *)
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

(* Marshal and unmarshal a compilation unit in linear format *)
type linear_item_info =
  | Func of Linear.fundecl
  | Data of Cmm.data_item list

type linear_unit_info =
  {
    mutable unit_name : string;
    mutable items : linear_item_info list;
    mutable for_pack : string option
  }

type error =
  | Wrong_format of string
  | Wrong_version of string
  | Corrupted of string
  | Marshal_failed of string

exception Error of error

let save filename linear_unit_info =
  let ch = open_out_bin filename in
  Misc.try_finally (fun () ->
    output_string ch Config.linear_magic_number;
    output_value ch linear_unit_info;
    (* Saved because Linearize and Emit depend on Cmm.label. *)
    output_value ch (Cmm.cur_label ());
    (* Compute digest of the contents and append it to the file. *)
    flush ch;
    let crc = Digest.file filename in
    output_value ch crc
  )
    ~always:(fun () -> close_out ch)
    ~exceptionally:(fun () -> raise (Error (Marshal_failed filename)))

let restore filename =
  let ic = open_in_bin filename in
  Misc.try_finally
    (fun () ->
       let magic = Config.linear_magic_number in
       let buffer = really_input_string ic (String.length magic) in
       if String.equal buffer magic then begin
         try
           let linear_unit_info = (input_value ic : linear_unit_info) in
           let last_label = (input_value ic : Cmm.label) in
           Cmm.reset ();
           Cmm.set_label last_label;
           let crc = (input_value ic : Digest.t) in
           linear_unit_info, crc
         with End_of_file | Failure _ -> raise (Error (Corrupted filename))
            | Error e -> raise (Error e)
       end
       else if String.sub buffer 0 9 = String.sub magic 0 9 then
         raise (Error (Wrong_version filename))
       else
         raise (Error (Wrong_format filename))
    )
    ~always:(fun () -> close_in ic)

(* Error report *)

open Format_doc

let report_error ppf = function
  | Wrong_format filename ->
      fprintf ppf "Expected Linear format. Incompatible file %a"
         Location.Doc.quoted_filename filename
  | Wrong_version filename ->
      fprintf ppf
        "%a@ is not compatible with this version of OCaml"
        Location.Doc.quoted_filename filename
  | Corrupted filename ->
      fprintf ppf "Corrupted format@ %a"
        Location.Doc.quoted_filename filename
  | Marshal_failed filename ->
      fprintf ppf "Failed to marshal Linear to file@ %a"
         Location.Doc.quoted_filename filename

let () =
  Location.register_error_of_exn
    (function
      | Error err -> Some (Location.error_of_printer_file report_error err)
      | _ -> None
    )
