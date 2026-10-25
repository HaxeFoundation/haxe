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

(* Compilation environments for compilation units *)

[@@@ocaml.warning "+a-4-9-40-41-42"]

open Config
open Cmx_format

type error =
    Not_a_unit_info of string
  | Corrupted_unit_info of string
  | Illegal_renaming of string * string * string
  | Mismatching_for_pack of string * string * string * string option

exception Error of error

let global_infos_table =
  (Hashtbl.create 17 : (string, unit_infos option) Hashtbl.t)
let export_infos_table =
  (Hashtbl.create 10 : (string, Export_info.t) Hashtbl.t)

let imported_sets_of_closures_table =
  (Set_of_closures_id.Tbl.create 10
   : Simple_value_approx.function_declarations option
       Set_of_closures_id.Tbl.t)

module CstMap =
  Map.Make(struct
    type t = Clambda.ustructured_constant
    let compare = Clambda.compare_structured_constants
    (* PR#6442: it is incorrect to use Stdlib.compare on values of type t
       because it compares "0.0" and "-0.0" equal. *)
  end)

module SymMap = Misc.Stdlib.String.Map

type structured_constants =
  {
    strcst_shared: string CstMap.t;
    strcst_all: Clambda.ustructured_constant SymMap.t;
  }

let structured_constants_empty  =
  {
    strcst_shared = CstMap.empty;
    strcst_all = SymMap.empty;
  }

let structured_constants = ref structured_constants_empty


let exported_constants = Hashtbl.create 17

let merged_environment = ref Export_info.empty

let default_ui_export_info =
  if Config.flambda then
    Cmx_format.Flambda Export_info.empty
  else
    Cmx_format.Clambda Value_unknown

let current_unit =
  { ui_name = "";
    ui_symbol = "";
    ui_defines = [];
    ui_imports_cmi = [];
    ui_imports_cmx = [];
    ui_curry_fun = [];
    ui_apply_fun = [];
    ui_send_fun = [];
    ui_force_link = false;
    ui_export_info = default_ui_export_info;
    ui_for_pack = None }

let symbol_separator =
  match Config.ccomp_type with
  | "msvc" -> '$' (* MASM does not allow for dots in symbol names *)
  | _ -> '.'

let concat_symbol unitname id =
  Printf.sprintf "%s%c%s" unitname symbol_separator id

let symbolname_for_pack pack name =
  match pack with
  | None -> name
  | Some p -> concat_symbol p name

let unit_id_from_name name = Ident.create_persistent name

let make_symbol ?(unitname = current_unit.ui_symbol) idopt =
  let prefix = "caml" ^ unitname in
  match idopt with
  | None -> prefix
  | Some id -> concat_symbol prefix id

let current_unit_linkage_name () =
  Linkage_name.create (make_symbol ~unitname:current_unit.ui_symbol None)

let reset ?packname name =
  Hashtbl.clear global_infos_table;
  Set_of_closures_id.Tbl.clear imported_sets_of_closures_table;
  let symbol = symbolname_for_pack packname name in
  current_unit.ui_name <- name;
  current_unit.ui_symbol <- symbol;
  current_unit.ui_defines <- [symbol];
  current_unit.ui_imports_cmi <- [];
  current_unit.ui_imports_cmx <- [];
  current_unit.ui_curry_fun <- [];
  current_unit.ui_apply_fun <- [];
  current_unit.ui_send_fun <- [];
  current_unit.ui_force_link <- !Clflags.link_everything;
  current_unit.ui_for_pack <- packname;
  Hashtbl.clear exported_constants;
  structured_constants := structured_constants_empty;
  current_unit.ui_export_info <- default_ui_export_info;
  merged_environment := Export_info.empty;
  Hashtbl.clear export_infos_table;
  let compilation_unit =
    Compilation_unit.create
      (Ident.create_persistent name)
      (current_unit_linkage_name ())
  in
  Compilation_unit.set_current compilation_unit

let current_unit_infos () =
  current_unit

let current_unit_name () =
  current_unit.ui_name

let symbol_in_current_unit name =
  let prefix = "caml" ^ current_unit.ui_symbol in
  name = prefix ||
  (let lp = String.length prefix in
   String.length name >= 2 + lp
   && String.sub name 0 lp = prefix
   && name.[lp] = '_'
   && name.[lp + 1] = '_')

let read_unit_info filename =
  let ic = open_in_bin filename in
  try
    let buffer = really_input_string ic (String.length cmx_magic_number) in
    if buffer <> cmx_magic_number then begin
      close_in ic;
      raise(Error(Not_a_unit_info filename))
    end;
    let ui = (input_value ic : unit_infos) in
    let crc = Digest.input ic in
    close_in ic;
    (ui, crc)
  with End_of_file | Failure _ ->
    close_in ic;
    raise(Error(Corrupted_unit_info(filename)))

let read_library_info filename =
  let ic = open_in_bin filename in
  let buffer = really_input_string ic (String.length cmxa_magic_number) in
  if buffer <> cmxa_magic_number then
    raise(Error(Not_a_unit_info filename));
  let infos = (input_value ic : library_infos) in
  close_in ic;
  infos


(* Read and cache info on global identifiers *)

(* Referring to a packed unit is only allowed from a unit that will
   ultimately end up in the same pack, including through nested packs. *)
let is_import_from_same_pack ~imported ~current =
  String.equal imported current
  || String.starts_with ~prefix:(imported ^ ".") current

let get_global_info global_ident = (
  let modname = Ident.name global_ident in
  if modname = current_unit.ui_name then
    Some current_unit
  else begin
    try
      Hashtbl.find global_infos_table modname
    with Not_found ->
      let (infos, crc) =
        if Env.is_imported_opaque modname then (None, None)
        else begin
          try
            let filename =
              Load_path.find_normalized (modname ^ ".cmx") in
            let (ui, crc) = read_unit_info filename in
            if ui.ui_name <> modname then
              raise(Error(Illegal_renaming(modname, ui.ui_name, filename)));
            (* Linking to a compilation unit expected to go into a
               pack (ui_for_pack = Some ...) is possible only from
               inside the same pack, but it is perfectly ok to link to
               an unit outside of the pack. *)
            (match ui.ui_for_pack, current_unit.ui_for_pack with
             | None, _ -> ()
             | Some p1, Some p2 when
                 is_import_from_same_pack ~imported:p1 ~current:p2 ->
                 ()
             | Some p1, p2 ->
               raise (Error (Mismatching_for_pack
                               (filename, p1, current_unit.ui_name, p2))));
            (Some ui, Some crc)
          with Not_found ->
            let warn = Warnings.No_cmx_file modname in
              Location.prerr_warning Location.none warn;
              (None, None)
          end
      in
      current_unit.ui_imports_cmx <-
        (modname, crc) :: current_unit.ui_imports_cmx;
      Hashtbl.add global_infos_table modname infos;
      infos
  end
)

let cache_unit_info ui =
  Hashtbl.add global_infos_table ui.ui_name (Some ui)

(* Return the approximation of a global identifier *)

let get_clambda_approx ui =
  assert(not Config.flambda);
  match ui.ui_export_info with
  | Flambda _ -> assert false
  | Clambda approx -> approx

let toplevel_approx :
  (string, Clambda.value_approximation) Hashtbl.t = Hashtbl.create 16

let record_global_approx_toplevel () =
  Hashtbl.add toplevel_approx current_unit.ui_name
    (get_clambda_approx current_unit)

let global_approx id =
  if Ident.is_predef id then Clambda.Value_unknown
  else try Hashtbl.find toplevel_approx (Ident.name id)
  with Not_found ->
    match get_global_info id with
      | None -> Clambda.Value_unknown
      | Some ui -> get_clambda_approx ui

(* Return the symbol used to refer to a global identifier *)

let symbol_for_global id =
  if Ident.is_predef id then
    "caml_exn_" ^ Ident.name id
  else begin
    let unitname = Ident.name id in
    match
      try ignore (Hashtbl.find toplevel_approx unitname); None
      with Not_found -> get_global_info id
    with
    | None -> make_symbol ~unitname:(Ident.name id) None
    | Some ui -> make_symbol ~unitname:ui.ui_symbol None
  end

(* Register the approximation of the module being compiled *)

let unit_for_global id =
  let sym_label = Linkage_name.create (symbol_for_global id) in
  Compilation_unit.create id sym_label

let predefined_exception_compilation_unit =
  Compilation_unit.create (Ident.create_persistent "__dummy__")
    (Linkage_name.create "__dummy__")

let is_predefined_exception sym =
  Compilation_unit.equal
    predefined_exception_compilation_unit
    (Symbol.compilation_unit sym)

let symbol_for_global' id =
  let sym_label = Linkage_name.create (symbol_for_global id) in
  if Ident.is_predef id then
    Symbol.of_global_linkage predefined_exception_compilation_unit sym_label
  else
    Symbol.of_global_linkage (unit_for_global id) sym_label

let set_global_approx approx =
  assert(not Config.flambda);
  current_unit.ui_export_info <- Clambda approx

(* Exporting and importing cross module information *)

let get_flambda_export_info ui =
  assert(Config.flambda);
  match ui.ui_export_info with
  | Clambda _ -> assert false
  | Flambda ei -> ei

let set_export_info export_info =
  assert(Config.flambda);
  current_unit.ui_export_info <- Flambda export_info

let approx_for_global comp_unit =
  let id = Compilation_unit.get_persistent_ident comp_unit in
  if (Compilation_unit.equal
      predefined_exception_compilation_unit
      comp_unit)
     || Ident.is_predef id
     || not (Ident.global id)
  then invalid_arg (Format.asprintf "approx_for_global %a" Ident.print id);
  let modname = Ident.name id in
  match Hashtbl.find export_infos_table modname with
  | otherwise -> Some otherwise
  | exception Not_found ->
    match get_global_info id with
    | None -> None
    | Some ui ->
      let exported = get_flambda_export_info ui in
      Hashtbl.add export_infos_table modname exported;
      merged_environment := Export_info.merge !merged_environment exported;
      Some exported

let approx_env () = !merged_environment

(* Record that a currying function or application function is needed *)

let need_curry_fun n =
  if not (List.mem n current_unit.ui_curry_fun) then
    current_unit.ui_curry_fun <- n :: current_unit.ui_curry_fun

let need_apply_fun n =
  assert(n > 0);
  if not (List.mem n current_unit.ui_apply_fun) then
    current_unit.ui_apply_fun <- n :: current_unit.ui_apply_fun

let need_send_fun n =
  if not (List.mem n current_unit.ui_send_fun) then
    current_unit.ui_send_fun <- n :: current_unit.ui_send_fun

(* Write the description of the current unit *)

let write_unit_info info filename =
  let oc = open_out_bin filename in
  output_string oc cmx_magic_number;
  output_value oc info;
  flush oc;
  let crc = Digest.file filename in
  Digest.output oc crc;
  close_out oc

let save_unit_info filename =
  current_unit.ui_imports_cmi <- Env.imports();
  write_unit_info current_unit filename

let current_unit () =
  match Compilation_unit.get_current () with
  | Some current_unit -> current_unit
  | None -> Misc.fatal_error "Compilenv.current_unit"

let current_unit_symbol () =
  Symbol.of_global_linkage (current_unit ()) (current_unit_linkage_name ())

let const_label = ref 0

let new_const_symbol () =
  incr const_label;
  make_symbol (Some (Int.to_string !const_label))

let snapshot () = !structured_constants
let backtrack s = structured_constants := s

let new_structured_constant cst ~shared =
  let {strcst_shared; strcst_all} = !structured_constants in
  if shared then
    try
      CstMap.find cst strcst_shared
    with Not_found ->
      let lbl = new_const_symbol() in
      structured_constants :=
        {
          strcst_shared = CstMap.add cst lbl strcst_shared;
          strcst_all = SymMap.add lbl cst strcst_all;
        };
      lbl
  else
    let lbl = new_const_symbol() in
    structured_constants :=
      {
        strcst_shared;
        strcst_all = SymMap.add lbl cst strcst_all;
      };
    lbl

let add_exported_constant s =
  Hashtbl.replace exported_constants s ()

let clear_structured_constants () =
  structured_constants := structured_constants_empty

let structured_constant_of_symbol s =
  SymMap.find_opt s (!structured_constants).strcst_all

let structured_constants () =
  let provenance : Clambda.usymbol_provenance =
    { original_idents = [];
      module_path =
        Path.Pident (Ident.create_persistent (current_unit_name ()));
    }
  in
  SymMap.bindings (!structured_constants).strcst_all
  |> List.map
    (fun (symbol, definition) ->
       {
         Clambda.symbol;
         exported = Hashtbl.mem exported_constants symbol;
         definition;
         provenance = Some provenance;
       })

let closure_symbol fv =
  let compilation_unit = Closure_id.get_compilation_unit fv in
  let unitname =
    Linkage_name.to_string (Compilation_unit.get_linkage_name compilation_unit)
  in
  let linkage_name =
    concat_symbol unitname ((Closure_id.unique_name fv) ^ "_closure")
  in
  Symbol.of_global_linkage compilation_unit (Linkage_name.create linkage_name)

let function_label fv =
  let compilation_unit = Closure_id.get_compilation_unit fv in
  let unitname =
    Linkage_name.to_string
      (Compilation_unit.get_linkage_name compilation_unit)
  in
  (concat_symbol unitname (Closure_id.unique_name fv))

let require_global global_ident =
  if not (Ident.is_predef global_ident) then
    ignore (get_global_info global_ident : Cmx_format.unit_infos option)

(* Error report *)

open Format_doc
module Style = Misc.Style

let report_error_doc ppf = function
  | Not_a_unit_info filename ->
      fprintf ppf "%a@ is not a compilation unit description."
        Location.Doc.quoted_filename filename
  | Corrupted_unit_info filename ->
      fprintf ppf "Corrupted compilation unit description@ %a"
       Location.Doc.quoted_filename filename
  | Illegal_renaming(name, modname, filename) ->
      fprintf ppf "%a@ contains the description for unit\
                   @ %a when %a was expected"
        Location.Doc.quoted_filename filename
        Style.inline_code name
        Style.inline_code modname
  | Mismatching_for_pack(filename, pack_1, current_unit, None) ->
      fprintf ppf "%a@ was built with %a, but the \
                   @ current unit %a is not"
        Location.Doc.quoted_filename filename
        Style.inline_code ("-for-pack " ^ pack_1)
        Style.inline_code current_unit
  | Mismatching_for_pack(filename, pack_1, current_unit, Some pack_2) ->
      fprintf ppf "%a@ was built with %a, but the \
                   @ current unit %a is built with %a"
        Location.Doc.quoted_filename filename
        Style.inline_code ("-for-pack " ^ pack_1)
        Style.inline_code current_unit
        Style.inline_code ("-for-pack " ^ pack_2)

let () =
  Location.register_error_of_exn
    (function
      | Error err -> Some (Location.error_of_printer_file report_error_doc err)
      | _ -> None
    )

let report_error = Format_doc.compat report_error_doc
