(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                         Alain Frisch, LexiFi                           *)
(*                                                                        *)
(*   Copyright 2012 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open Asttypes
open Parsetree
open Ast_helper


module Attribute_table = Hashtbl.Make (struct
  type t = string with_loc

  let hash : t -> int = Hashtbl.hash
  let equal : t -> t -> bool = (=)
end)
let unused_attrs = Attribute_table.create 128
let mark_used t = Attribute_table.remove unused_attrs t

(* [attr_order] is used to issue unused attribute warnings in the order the
   attributes occur in the file rather than the random order of the hash table
*)
let attr_order a1 a2 =
  match String.compare a1.loc.loc_start.pos_fname a2.loc.loc_start.pos_fname
  with
  | 0 -> Int.compare a1.loc.loc_start.pos_cnum a2.loc.loc_start.pos_cnum
  | n -> n

let compiler_stops_before_attributes_consumed () =
  let stops_before_lambda =
    match !Clflags.stop_after with
    | None -> false
    | Some pass -> Clflags.Compiler_pass.(compare pass Lambda) < 0
  in
  stops_before_lambda || !Clflags.print_types

let warn_unused () =
  let keys = List.of_seq (Attribute_table.to_seq_keys unused_attrs) in
  Attribute_table.clear unused_attrs;
  if not (compiler_stops_before_attributes_consumed ()) then
    let keys = List.sort attr_order keys in
    List.iter (fun sloc ->
      Location.prerr_warning sloc.loc (Warnings.Misplaced_attribute sloc.txt))
      keys

(* These are the attributes that are tracked in the builtin_attrs table for
   misplaced attribute warnings. *)
let builtin_attrs =
  [ "alert"
  ; "boxed"
  ; "deprecated"
  ; "deprecated_mutable"
  ; "explicit_arity"
  ; "immediate"
  ; "immediate64"
  ; "inline"
  ; "inlined"
  ; "noalloc"
  ; "poll"
  ; "ppwarning"
  ; "specialise"
  ; "specialised"
  ; "tailcall"
  ; "tail_mod_cons"
  ; "unboxed"
  ; "untagged"
  ; "unrolled"
  ; "warnerror"
  ; "warning"
  ; "warn_on_literal_pattern"
  ]

let builtin_attrs =
  let tbl = Hashtbl.create 128 in
  List.iter (fun attr -> Hashtbl.add tbl attr ()) builtin_attrs;
  tbl

let drop_ocaml_attr_prefix s =
  let len = String.length s in
  if String.starts_with ~prefix:"ocaml." s && len > 6 then
    String.sub s 6 (len - 6)
  else
    s

let is_builtin_attr s = Hashtbl.mem builtin_attrs (drop_ocaml_attr_prefix s)

type current_phase = Parser | Invariant_check

let register_attr current_phase name =
  match current_phase with
  | Parser when !Clflags.all_ppx <> [] -> ()
  | Parser | Invariant_check ->
    if is_builtin_attr name.txt then
      Attribute_table.replace unused_attrs name ()

let string_of_cst const =
  match const.pconst_desc with
  | Pconst_string(s, _, _) -> Some s
  | _ -> None

let string_of_payload = function
  | PStr[{pstr_desc=Pstr_eval({pexp_desc=Pexp_constant c},_)}] ->
      string_of_cst c
  | _ -> None

let string_of_opt_payload p =
  match string_of_payload p with
  | Some s -> s
  | None -> ""

module Style = Misc.Style
let error_of_extension ext =
  let submessage_from main_loc main_txt = function
    | {pstr_desc=Pstr_extension
           (({txt = ("ocaml.error"|"error"); loc}, p), _)} ->
        begin match p with
        | PStr([{pstr_desc=Pstr_eval
                     ({pexp_desc=Pexp_constant
                           {pconst_desc=Pconst_string(msg, _, _); _}}, _)}
               ]) ->
            Location.msg ~loc "%a" Format_doc.pp_print_text msg
        | _ ->
            Location.msg ~loc "Invalid syntax for sub-message of extension %a."
              Style.inline_code main_txt
        end
    | {pstr_desc=Pstr_extension (({txt; loc}, _), _)} ->
        Location.msg ~loc "Uninterpreted extension '%a'."
          Style.inline_code txt
    | _ ->
        Location.msg ~loc:main_loc
          "Invalid syntax for sub-message of extension %a."
          Style.inline_code main_txt
  in
  match ext with
  | ({txt = ("ocaml.error"|"error") as txt; loc}, p) ->
      begin match p with
      | PStr [] -> raise Location.Already_displayed_error
      | PStr({pstr_desc=Pstr_eval
                  ({pexp_desc=Pexp_constant
                      {pconst_desc=Pconst_string(msg, _, _)}}, _)}::
             inner) ->
          let sub = List.map (submessage_from loc txt) inner in
          Location.error_of_printer ~loc ~sub Format_doc.pp_print_text msg
      | _ ->
          Location.errorf ~loc "Invalid syntax for extension '%s'." txt
      end
  | ({txt; loc}, _) ->
      Location.errorf ~loc "Uninterpreted extension '%s'." txt

let attr_equals_builtin {attr_name = {txt; _}; _} s =
  (* Check for attribute s or ocaml.s.  Avoid allocating a fresh string. *)
  txt = s ||
  (   String.length txt = 6 + String.length s
   && String.starts_with ~prefix:"ocaml." txt
   && String.ends_with ~suffix:s txt)

let mark_alert_used a =
  if attr_equals_builtin a "deprecated" || attr_equals_builtin a "alert"
  then mark_used a.attr_name

let mark_alerts_used l = List.iter mark_alert_used l

let mark_warn_on_literal_pattern_used l =
  List.iter (fun a ->
    if attr_equals_builtin a "warn_on_literal_pattern"
    then mark_used a.attr_name)
    l

let mark_deprecated_mutable_used l =
  List.iter (fun a ->
    if attr_equals_builtin a "deprecated_mutable"
    then mark_used a.attr_name)
    l

let mark_payload_attrs_used payload =
  let iter =
    { Ast_iterator.default_iterator
      with attribute = fun self a ->
        mark_used a.attr_name;
        Ast_iterator.default_iterator.attribute self a
    }
  in
  iter.payload iter payload

let kind_and_message = function
  | PStr[
      {pstr_desc=
         Pstr_eval
           ({pexp_desc=Pexp_apply
                 ({pexp_desc=Pexp_ident{txt=Longident.Lident id}},
                  [Nolabel,{pexp_desc=Pexp_constant
                                {pconst_desc=Pconst_string(s,_,_); _}}])
            },_)}] ->
      Some (id, s)
  | PStr[
      {pstr_desc=
         Pstr_eval
           ({pexp_desc=Pexp_ident{txt=Longident.Lident id}},_)}] ->
      Some (id, "")
  | _ -> None

let cat s1 s2 =
  if s2 = "" then s1 else s1 ^ "\n" ^ s2

let alert_attr x =
  if attr_equals_builtin x "deprecated" then
    Some (x, "deprecated", string_of_opt_payload x.attr_payload)
  else if attr_equals_builtin x "alert" then
    begin match kind_and_message x.attr_payload with
    | Some (kind, message) -> Some (x, kind, message)
    | None -> None (* note: bad payloads detected by warning_attribute *)
    end
  else None

let alert_attrs l =
  List.filter_map alert_attr l

let alerts_of_attrs l =
  List.fold_left
    (fun acc (_, kind, message) ->
       let upd = function
         | None | Some "" -> Some message
         | Some s -> Some (cat s message)
       in
       Misc.Stdlib.String.Map.update kind upd acc
    )
    Misc.Stdlib.String.Map.empty
    (alert_attrs l)

let check_alerts loc attrs s =
  Misc.Stdlib.String.Map.iter
    (fun kind message -> Location.alert loc ~kind (cat s message))
    (alerts_of_attrs attrs)

let check_alerts_inclusion ~def ~use loc attrs1 attrs2 s =
  let m2 = alerts_of_attrs attrs2 in
  Misc.Stdlib.String.Map.iter
    (fun kind msg ->
       if not (Misc.Stdlib.String.Map.mem kind m2) then
         Location.alert ~def ~use ~kind loc (cat s msg)
    )
    (alerts_of_attrs attrs1)

let rec deprecated_mutable_of_attrs = function
  | [] -> None
  | attr :: _ when attr_equals_builtin attr "deprecated_mutable" ->
    Some (string_of_opt_payload attr.attr_payload)
  | _ :: tl -> deprecated_mutable_of_attrs tl

let check_deprecated_mutable loc attrs s =
  match deprecated_mutable_of_attrs attrs with
  | None -> ()
  | Some txt ->
      Location.deprecated loc (Printf.sprintf "mutating field %s" (cat s txt))

let check_deprecated_mutable_inclusion ~def ~use loc attrs1 attrs2 s =
  match deprecated_mutable_of_attrs attrs1,
        deprecated_mutable_of_attrs attrs2
  with
  | None, _ | Some _, Some _ -> ()
  | Some txt, None ->
      Location.deprecated ~def ~use loc
        (Printf.sprintf "mutating field %s" (cat s txt))

let rec attrs_of_sig = function
  | {psig_desc = Psig_attribute a} :: tl ->
      a :: attrs_of_sig tl
  | _ ->
      []

let alerts_of_sig ~mark sg =
  let a = attrs_of_sig sg in
  if mark then mark_alerts_used a;
  alerts_of_attrs a

let rec attrs_of_str = function
  | {pstr_desc = Pstr_attribute a} :: tl ->
      a :: attrs_of_str tl
  | _ ->
      []

let alerts_of_str ~mark str =
  let a = attrs_of_str str in
  if mark then mark_alerts_used a;
  alerts_of_attrs a

let warn_payload loc txt msg =
  Location.prerr_warning loc (Warnings.Attribute_payload (txt, msg))

let warning_attribute ?(ppwarning = true) =
  let process loc name errflag payload =
    mark_used name;
    match string_of_payload payload with
    | Some s ->
        begin try
          Option.iter (Location.prerr_alert loc)
            (Warnings.parse_options errflag s)
        with Arg.Bad msg -> warn_payload loc name.txt msg
        end
    | None ->
        warn_payload loc name.txt "A single string literal is expected"
  in
  let process_alert loc name = function
    | PStr[{pstr_desc=
              Pstr_eval(
                {pexp_desc=Pexp_constant {pconst_desc=Pconst_string(s,_,_); _}},
                _)
           }] ->
        begin
          mark_used name;
          try Warnings.parse_alert_option s
          with Arg.Bad msg -> warn_payload loc name.txt msg
        end
    | k ->
        match kind_and_message k with
        | Some ("all", _) ->
            warn_payload loc name.txt "The alert name 'all' is reserved"
        | Some _ ->
            (* Do [mark_used] in the [Some] case only if Warning 53 is
               disabled. Later, they will be marked used (provided they are in a
               valid place) in [compile_common], when they are extracted to be
               persisted inside the [.cmi] file. *)
            if not (Warnings.is_active (Misplaced_attribute ""))
            then mark_used name
        | None -> begin
            (* Do [mark_used] in the [None] case, which is just malformed and
               covered by the "Invalid payload" warning. *)
            mark_used name;
            warn_payload loc name.txt "Invalid payload"
          end
  in
  fun ({attr_name; attr_loc; attr_payload} as attr) ->
    if attr_equals_builtin attr "warning" then
      process attr_loc attr_name false attr_payload
    else if attr_equals_builtin attr "warnerror" then
      process attr_loc attr_name true attr_payload
    else if attr_equals_builtin attr "alert" then
      process_alert attr_loc attr_name attr_payload
    else if ppwarning && attr_equals_builtin attr "ppwarning" then
      begin match attr_payload with
      | PStr [{ pstr_desc=
                  Pstr_eval({pexp_desc=Pexp_constant
                                 {pconst_desc=Pconst_string (s, _, _); _}},_);
                pstr_loc }] ->
        (mark_used attr_name;
         Location.prerr_warning pstr_loc (Warnings.Preprocessor s))
      | _ ->
        (mark_used attr_name;
         warn_payload attr_loc attr_name.txt
           "A single string literal is expected")
      end

let warning_scope ?ppwarning attrs f =
  let prev = Warnings.backup () in
  try
    List.iter (warning_attribute ?ppwarning) (List.rev attrs);
    let ret = f () in
    Warnings.restore prev;
    ret
  with exn ->
    Warnings.restore prev;
    raise exn

let has_attribute nm attrs =
  List.exists
    (fun a ->
       if attr_equals_builtin a nm
       then (mark_used a.attr_name; true)
       else false)
    attrs

type attr_action = Mark_used_only | Return
let select_attributes actions attrs =
  List.filter (fun a ->
    List.exists (fun (nm, action) ->
      attr_equals_builtin a nm &&
      begin
        mark_used a.attr_name;
        action = Return
      end)
      actions
  ) attrs

let warn_on_literal_pattern attrs =
  has_attribute "warn_on_literal_pattern" attrs

let explicit_arity attrs = has_attribute "explicit_arity" attrs

let immediate attrs = has_attribute "immediate" attrs

let immediate64 attrs = has_attribute "immediate64" attrs

(* The "ocaml.boxed (default)" and "ocaml.unboxed (default)"
   attributes cannot be input by the user, they are added by the
   compiler when applying the default setting. This is done to record
   in the .cmi the default used by the compiler when compiling the
   source file because the default can change between compiler
   invocations. *)

let has_unboxed attrs = has_attribute "unboxed" attrs

let has_boxed attrs = has_attribute "boxed" attrs
