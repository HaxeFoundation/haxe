(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*   Xavier Leroy, projet Gallium, INRIA Rocquencourt                     *)
(*   Gabriel Scherer, projet Parsifal, INRIA Saclay                       *)
(*                                                                        *)
(*   Copyright 2019 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Persistent structure descriptions *)

open Misc
open Cmi_format

module Consistbl = Consistbl.Make (Misc.Stdlib.String)

let add_delayed_check_forward = ref (fun _ -> assert false)

type error =
  | Illegal_renaming of modname * modname * filepath
  | Inconsistent_import of modname * filepath * filepath
  | Need_recursive_types of modname

exception Error of error
let error err = raise (Error err)

module Persistent_signature = struct
  type t =
    { filename : string;
      cmi : Cmi_format.cmi_infos;
      visibility : Load_path.visibility }

  let load = ref (fun ~allow_hidden ~unit_name ->
    match Load_path.find_normalized_with_visibility (unit_name ^ ".cmi") with
    | filename, visibility when allow_hidden ->
      Some { filename; cmi = read_cmi filename; visibility}
    | filename, Visible ->
      Some { filename; cmi = read_cmi filename; visibility = Visible}
    | _, Hidden
    | exception Not_found -> None)
end

type can_load_cmis =
  | Can_load_cmis
  | Cannot_load_cmis of Lazy_backtrack.log

type pers_struct = {
  ps_name: string;
  ps_crcs: (string * Digest.t option) list;
  ps_filename: string;
  ps_flags: pers_flags list;
  ps_visibility: Load_path.visibility;
}

module String = Misc.Stdlib.String

(* If a .cmi file is missing (or invalid), we
   store it as Missing in the cache. *)
type 'a pers_struct_info =
  | Missing
  | Found of pers_struct * 'a

type 'a t = {
  persistent_structures : (string, 'a pers_struct_info) Hashtbl.t;
  imported_units: String.Set.t ref;
  imported_opaque_units: String.Set.t ref;
  crc_units: Consistbl.t;
  can_load_cmis: can_load_cmis ref;
}

let empty () = {
  persistent_structures = Hashtbl.create 17;
  imported_units = ref String.Set.empty;
  imported_opaque_units = ref String.Set.empty;
  crc_units = Consistbl.create ();
  can_load_cmis = ref Can_load_cmis;
}

let clear penv =
  let {
    persistent_structures;
    imported_units;
    imported_opaque_units;
    crc_units;
    can_load_cmis;
  } = penv in
  Hashtbl.clear persistent_structures;
  imported_units := String.Set.empty;
  imported_opaque_units := String.Set.empty;
  Consistbl.clear crc_units;
  can_load_cmis := Can_load_cmis;
  ()

let clear_missing {persistent_structures; _} =
  let missing_entries =
    Hashtbl.fold
      (fun name r acc -> if r = Missing then name :: acc else acc)
      persistent_structures []
  in
  List.iter (Hashtbl.remove persistent_structures) missing_entries

let add_import {imported_units; _} s =
  imported_units := String.Set.add s !imported_units

let register_import_as_opaque {imported_opaque_units; _} s =
  imported_opaque_units := String.Set.add s !imported_opaque_units

let find_in_cache {persistent_structures; _} s =
  match Hashtbl.find persistent_structures s with
  | exception Not_found -> None
  | Missing -> None
  | Found (_ps, pm) -> Some pm

let import_crcs penv ~source crcs =
  let {crc_units; _} = penv in
  let import_crc (name, crco) =
    match crco with
    | None -> ()
    | Some crc ->
        add_import penv name;
        Consistbl.check crc_units name crc source
  in List.iter import_crc crcs

let check_consistency penv ps =
  try import_crcs penv ~source:ps.ps_filename ps.ps_crcs
  with Consistbl.Inconsistency {
      unit_name = name;
      inconsistent_source = source;
      original_source = auth;
    } ->
    error (Inconsistent_import(name, auth, source))

let can_load_cmis penv =
  !(penv.can_load_cmis)
let set_can_load_cmis penv setting =
  penv.can_load_cmis := setting

let without_cmis penv f x =
  let log = Lazy_backtrack.log () in
  let res =
    Misc.(protect_refs
            [R (penv.can_load_cmis, Cannot_load_cmis log)]
            (fun () -> f x))
  in
  Lazy_backtrack.backtrack log;
  res

let fold {persistent_structures; _} f x =
  Hashtbl.fold (fun modname pso x -> match pso with
      | Missing -> x
      | Found (_, pm) -> f modname pm x)
    persistent_structures x

(* Reading persistent structures from .cmi files *)

let save_pers_struct penv crc ps pm =
  let {persistent_structures; crc_units; _} = penv in
  let modname = ps.ps_name in
  Hashtbl.add persistent_structures modname (Found (ps, pm));
  List.iter
    (function
        | Rectypes -> ()
        | Alerts _ -> ()
        | Opaque -> register_import_as_opaque penv modname)
    ps.ps_flags;
  Consistbl.check crc_units modname crc ps.ps_filename;
  add_import penv modname

let acknowledge_pers_struct penv check modname pers_sig pm =
  let { Persistent_signature.filename; cmi; visibility } = pers_sig in
  let name = cmi.cmi_name in
  let crcs = cmi.cmi_crcs in
  let flags = cmi.cmi_flags in
  let ps = { ps_name = name;
             ps_crcs = crcs;
             ps_filename = filename;
             ps_flags = flags;
             ps_visibility = visibility;
           } in
  if ps.ps_name <> modname then
    error (Illegal_renaming(modname, ps.ps_name, filename));
  List.iter
    (function
        | Rectypes ->
            if not !Clflags.recursive_types then
              error (Need_recursive_types(ps.ps_name))
        | Alerts _ -> ()
        | Opaque -> register_import_as_opaque penv modname)
    ps.ps_flags;
  if check then check_consistency penv ps;
  let {persistent_structures; _} = penv in
  Hashtbl.add persistent_structures modname (Found (ps, pm));
  ps

let read_pers_struct penv val_of_pers_sig check cmi =
  let modname = Unit_info.Artifact.modname cmi in
  let filename = Unit_info.Artifact.filename cmi in
  add_import penv modname;
  let cmi = read_cmi filename in
  let pers_sig = { Persistent_signature.filename; cmi; visibility = Visible } in
  let pm = val_of_pers_sig pers_sig in
  let ps = acknowledge_pers_struct penv check modname pers_sig pm in
  (ps, pm)

let find_pers_struct ~allow_hidden penv val_of_pers_sig check name =
  let {persistent_structures; _} = penv in
  if name = "*predef*" then raise Not_found;
  match Hashtbl.find persistent_structures name with
  | Found (ps, pm) when allow_hidden || ps.ps_visibility = Load_path.Visible ->
    (ps, pm)
  | Found _ -> raise Not_found
  | Missing -> raise Not_found
  | exception Not_found ->
    match can_load_cmis penv with
    | Cannot_load_cmis _ -> raise Not_found
    | Can_load_cmis ->
        let psig =
          match !Persistent_signature.load ~allow_hidden ~unit_name:name with
          | Some psig -> psig
          | None ->
            if allow_hidden then Hashtbl.add persistent_structures name Missing;
            raise Not_found
        in
        add_import penv name;
        let pm = val_of_pers_sig psig in
        let ps = acknowledge_pers_struct penv check name psig pm in
        (ps, pm)

module Style = Misc.Style
(* Emits a warning if there is no valid cmi for name *)
let check_pers_struct ~allow_hidden penv f ~loc name =
  try
    ignore (find_pers_struct ~allow_hidden penv f false name)
  with
  | Not_found ->
      let warn = Warnings.No_cmi_file(name, None) in
        Location.prerr_warning loc warn
  | Cmi_format.Error err ->
      let msg = Format.asprintf "%a"
          Cmi_format.report_error err in
      let warn = Warnings.No_cmi_file(name, Some msg) in
        Location.prerr_warning loc warn
  | Error err ->
      let msg =
        match err with
        | Illegal_renaming(name, ps_name, filename) ->
            Format_doc.doc_printf
              " %a@ contains the compiled interface for @ \
               %a when %a was expected"
              Location.Doc.quoted_filename filename
              Style.inline_code ps_name
              Style.inline_code name
        | Inconsistent_import _ -> assert false
        | Need_recursive_types name ->
            Format_doc.doc_printf
              "%a uses recursive types"
              Style.inline_code name
      in
      let msg = Format_doc.(asprintf "%a" pp_doc) msg in
      let warn = Warnings.No_cmi_file(name, Some msg) in
        Location.prerr_warning loc warn

let read penv f a =
  snd (read_pers_struct penv f true a)

let find ~allow_hidden penv f name =
  snd (find_pers_struct ~allow_hidden penv f true name)

let check ~allow_hidden penv f ~loc name =
  let {persistent_structures; _} = penv in
  if not (Hashtbl.mem persistent_structures name) then begin
    (* PR#6843: record the weak dependency ([add_import]) regardless of
       whether the check succeeds, to help make builds more
       deterministic. *)
    add_import penv name;
    if (Warnings.is_active (Warnings.No_cmi_file("", None))) then
      !add_delayed_check_forward
        (fun () -> check_pers_struct ~allow_hidden penv f ~loc name)
  end

let crc_of_unit penv f name =
  let (ps, _pm) = find_pers_struct ~allow_hidden:true penv f true name in
  let crco =
    try
      List.assoc name ps.ps_crcs
    with Not_found ->
      assert false
  in
    match crco with
      None -> assert false
    | Some crc -> crc

let imports {imported_units; crc_units; _} =
  Consistbl.extract (String.Set.elements !imported_units) crc_units

let looked_up {persistent_structures; _} modname =
  Hashtbl.mem persistent_structures modname

let is_imported {imported_units; _} s =
  String.Set.mem s !imported_units

let is_imported_opaque {imported_opaque_units; _} s =
  String.Set.mem s !imported_opaque_units

let make_cmi penv modname sign alerts =
  let flags =
    List.concat [
      if !Clflags.recursive_types then [Cmi_format.Rectypes] else [];
      if !Clflags.opaque then [Cmi_format.Opaque] else [];
      [Alerts alerts];
    ]
  in
  let crcs = imports penv in
  {
    cmi_name = modname;
    cmi_sign = sign;
    cmi_crcs = crcs;
    cmi_flags = flags
  }

let save_cmi penv psig pm =
  let { Persistent_signature.filename; cmi; visibility } = psig in
  Misc.try_finally (fun () ->
      let {
        cmi_name = modname;
        cmi_sign = _;
        cmi_crcs = imports;
        cmi_flags = flags;
      } = cmi in
      let crc =
        output_to_file_via_temporary (* see MPR#7472, MPR#4991 *)
          ~mode: [Open_binary] filename
          (fun temp_filename oc -> output_cmi temp_filename oc cmi) in
      (* Enter signature in persistent table so that imports()
         will also return its crc *)
      let ps =
        { ps_name = modname;
          ps_crcs = (cmi.cmi_name, Some crc) :: imports;
          ps_filename = filename;
          ps_flags = flags;
          ps_visibility = visibility
        } in
      save_pers_struct penv crc ps pm
    )
    ~exceptionally:(fun () -> remove_file filename)

let report_error_doc ppf =
  let open Format_doc in
  function
  | Illegal_renaming(modname, ps_name, filename) -> fprintf ppf
      "Wrong file naming: %a@ contains the compiled interface for@ \
       %a when %a was expected"
      Location.Doc.quoted_filename filename
      Style.inline_code ps_name
      Style.inline_code modname
  | Inconsistent_import(name, source1, source2) -> fprintf ppf
      "@[<hov>The files %a@ and %a@ \
              make inconsistent assumptions@ over interface %a@]"
      Location.Doc.quoted_filename source1
      Location.Doc.quoted_filename source2
      Style.inline_code name
  | Need_recursive_types(import) ->
      fprintf ppf
        "@[<hov>Invalid import of %a, which uses recursive types.@ \
         The compilation flag %a is required@]"
        Style.inline_code import
        Style.inline_code "-rectypes"

let () =
  Location.register_error_of_exn
    (function
      | Error err ->
          Some (Location.error_of_printer_file report_error_doc err)
      | _ -> None
    )

let report_error = Format_doc.compat report_error_doc
