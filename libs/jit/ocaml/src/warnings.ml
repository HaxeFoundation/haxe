(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Pierre Weis && Damien Doligez, INRIA Rocquencourt          *)
(*                                                                        *)
(*   Copyright 1998 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* When you change this, you need to update:
   - the list 'description' at the bottom of this file
   - man/ocamlc.m
*)

type loc = {
  loc_start: Lexing.position;
  loc_end: Lexing.position;
  loc_ghost: bool;
}

type field_usage_warning =
  | Unused
  | Not_read
  | Not_mutated

type constructor_usage_warning =
  | Unused
  | Not_constructed
  | Only_exported_private

type t =
  | Comment_start                           (*  1 *)
  | Comment_not_end                         (*  2 *)
(*| Deprecated --> alert "deprecated" *)    (*  3 *)
  | Fragile_match of string                 (*  4 *)
  | Ignored_partial_application             (*  5 *)
  | Labels_omitted of string list           (*  6 *)
  | Method_override of string list          (*  7 *)
  | Partial_match of string                 (*  8 *)
  | Missing_record_field_pattern of string  (*  9 *)
  | Non_unit_statement                      (* 10 *)
  | Redundant_case                          (* 11 *)
  | Redundant_subpat                        (* 12 *)
  | Instance_variable_override of string list (* 13 *)
  | Illegal_backslash                       (* 14 *)
  | Implicit_public_methods of string list  (* 15 *)
  | Unerasable_optional_argument            (* 16 *)
  | Undeclared_virtual_method of string     (* 17 *)
  | Not_principal of Format_doc.t           (* 18 *)
  | Non_principal_labels of string          (* 19 *)
  | Ignored_extra_argument                  (* 20 *)
  | Nonreturning_statement                  (* 21 *)
  | Preprocessor of string                  (* 22 *)
  | Useless_record_with                     (* 23 *)
  | Bad_module_name of string               (* 24 *)
  | All_clauses_guarded                     (* 8, used to be 25 *)
  | Unused_var of string                    (* 26 *)
  | Unused_var_strict of string             (* 27 *)
  | Wildcard_arg_to_constant_constr         (* 28 *)
  | Eol_in_string                           (* 29 *)
  | Duplicate_definitions of string * string * string * string (*30 *)
  (* [Module_linked_twice of string * string * string] (* 31 *)
     was turned into a hard error *)
  | Unused_value_declaration of string      (* 32 *)
  | Unused_open of string                   (* 33 *)
  | Unused_type_declaration of string       (* 34 *)
  | Unused_for_index of string              (* 35 *)
  | Unused_ancestor of string               (* 36 *)
  | Unused_constructor of string * constructor_usage_warning (* 37 *)
  | Unused_extension of string * bool * constructor_usage_warning (* 38 *)
  | Unused_rec_flag                         (* 39 *)
  | Name_out_of_scope of string * string list * bool (* 40 *)
  | Ambiguous_name of string list * string list *  bool * string (* 41 *)
  | Disambiguated_name of string            (* 42 *)
  | Nonoptional_label of string             (* 43 *)
  | Open_shadow_identifier of string * string (* 44 *)
  | Open_shadow_label_constructor of string * string (* 45 *)
  | Bad_env_variable of string * string     (* 46 *)
  | Attribute_payload of string * string    (* 47 *)
  | Eliminated_optional_arguments of string list (* 48 *)
  | No_cmi_file of string * string option   (* 49 *)
  | Unexpected_docstring of bool            (* 50 *)
  | Wrong_tailcall_expectation of bool      (* 51 *)
  | Fragile_literal_pattern                 (* 52 *)
  | Misplaced_attribute of string           (* 53 *)
  | Duplicated_attribute of string          (* 54 *)
  | Inlining_impossible of string           (* 55 *)
  | Unreachable_case                        (* 56 *)
  | Ambiguous_var_in_pattern_guard of string list (* 57 *)
  | No_cmx_file of string                   (* 58 *)
  | Flambda_assignment_to_non_mutable_value (* 59 *)
  | Unused_module of string                 (* 60 *)
  | Unboxable_type_in_prim_decl of string   (* 61 *)
  | Constraint_on_gadt                      (* 62 *)
  | Erroneous_printed_signature of string   (* 63 *)
  | Unsafe_array_syntax_without_parsing     (* 64 *)
  | Redefining_unit of string               (* 65 *)
  | Unused_open_bang of string              (* 66 *)
  | Unused_functor_parameter of string      (* 67 *)
  | Match_on_mutable_state_prevent_uncurry  (* 68 *)
  | Unused_field of string * field_usage_warning (* 69 *)
  | Missing_mli                             (* 70 *)
  | Unused_tmc_attribute                    (* 71 *)
  | Tmc_breaks_tailcall                     (* 72 *)
  | Generative_application_expects_unit     (* 73 *)
  | Degraded_to_partial_match               (* 74 *)

(* If you remove a warning, leave a hole in the numbering.  NEVER change
   the numbers of existing warnings.
   If you add a new warning, add it at the end with a new number;
   do NOT reuse one of the holes.
*)

type alert = {kind:string; message:string; def:loc; use:loc}

let number = function
  | Comment_start -> 1
  | Comment_not_end -> 2
  | Fragile_match _ -> 4
  | Ignored_partial_application -> 5
  | Labels_omitted _ -> 6
  | Method_override _ -> 7
  | Partial_match _ -> 8
  | Missing_record_field_pattern _ -> 9
  | Non_unit_statement -> 10
  | Redundant_case -> 11
  | Redundant_subpat -> 12
  | Instance_variable_override _ -> 13
  | Illegal_backslash -> 14
  | Implicit_public_methods _ -> 15
  | Unerasable_optional_argument -> 16
  | Undeclared_virtual_method _ -> 17
  | Not_principal _ -> 18
  | Non_principal_labels _ -> 19
  | Ignored_extra_argument -> 20
  | Nonreturning_statement -> 21
  | Preprocessor _ -> 22
  | Useless_record_with -> 23
  | Bad_module_name _ -> 24
  | All_clauses_guarded -> 8 (* used to be 25 *)
  | Unused_var _ -> 26
  | Unused_var_strict _ -> 27
  | Wildcard_arg_to_constant_constr -> 28
  | Eol_in_string -> 29
  | Duplicate_definitions _ -> 30
  | Unused_value_declaration _ -> 32
  | Unused_open _ -> 33
  | Unused_type_declaration _ -> 34
  | Unused_for_index _ -> 35
  | Unused_ancestor _ -> 36
  | Unused_constructor _ -> 37
  | Unused_extension _ -> 38
  | Unused_rec_flag -> 39
  | Name_out_of_scope _ -> 40
  | Ambiguous_name _ -> 41
  | Disambiguated_name _ -> 42
  | Nonoptional_label _ -> 43
  | Open_shadow_identifier _ -> 44
  | Open_shadow_label_constructor _ -> 45
  | Bad_env_variable _ -> 46
  | Attribute_payload _ -> 47
  | Eliminated_optional_arguments _ -> 48
  | No_cmi_file _ -> 49
  | Unexpected_docstring _ -> 50
  | Wrong_tailcall_expectation _ -> 51
  | Fragile_literal_pattern -> 52
  | Misplaced_attribute _ -> 53
  | Duplicated_attribute _ -> 54
  | Inlining_impossible _ -> 55
  | Unreachable_case -> 56
  | Ambiguous_var_in_pattern_guard _ -> 57
  | No_cmx_file _ -> 58
  | Flambda_assignment_to_non_mutable_value -> 59
  | Unused_module _ -> 60
  | Unboxable_type_in_prim_decl _ -> 61
  | Constraint_on_gadt -> 62
  | Erroneous_printed_signature _ -> 63
  | Unsafe_array_syntax_without_parsing -> 64
  | Redefining_unit _ -> 65
  | Unused_open_bang _ -> 66
  | Unused_functor_parameter _ -> 67
  | Match_on_mutable_state_prevent_uncurry -> 68
  | Unused_field _ -> 69
  | Missing_mli -> 70
  | Unused_tmc_attribute -> 71
  | Tmc_breaks_tailcall -> 72
  | Generative_application_expects_unit -> 73
  | Degraded_to_partial_match -> 74
;;
(* DO NOT REMOVE the ;; above: it is used by
   the testsuite/ests/warnings/mnemonics.mll test to determine where
   the  definition of the number function above ends *)

let last_warning_number = 74

type description =
  { number : int;
    names : string list;
    (* The first element of the list is the current name, any following ones are
       deprecated. The current name should always be derived mechanically from
       the constructor name. *)
    description : string;
    since : Sys.ocaml_release_info option;
    (* The compiler version introducing this warning; only tagged for warnings
       created after 3.12, which introduced the numbered syntax. *)
  }

let since major minor = Some { Sys.major; minor; patchlevel=0; extra=None }

let descriptions = [
  { number = 1;
    names = ["comment-start"];
    description = "Suspicious-looking start-of-comment mark.";
    since = None };
  { number = 2;
    names =  ["comment-not-end"];
    description = "Suspicious-looking end-of-comment mark.";
    since = None };
  { number = 3;
    names = [];
    description = "Deprecated synonym for the 'deprecated' alert.";
    since = None };
  { number = 4;
    names = ["fragile-match"];
    description =
      "Fragile pattern matching: matching that will remain complete even\n\
      \    if additional constructors are added to one of the variant types\n\
      \    matched.";
    since = None };
  { number = 5;
    names = ["ignored-partial-application"];
    description =
      "Partially applied function: expression whose result has function\n\
      \    type and is ignored.";
    since = None };
  { number = 6;
    names = ["labels-omitted"];
    description = "Label omitted in function application.";
    since = None };
  { number = 7;
    names = ["method-override"];
    description = "Method overridden.";
    since = None };
  { number = 8;
    names = ["partial-match"];
    description = "Partial match: missing cases in pattern-matching.";
    since = None };
  { number = 9;
    names = ["missing-record-field-pattern"];
    description = "Missing fields in a record pattern.";
    since = None };
  { number = 10;
    names = ["non-unit-statement"];
    description =
      "Expression on the left-hand side of a sequence that doesn't have type\n\
      \    \"unit\" (and that is not a function, see warning number 5).";
    since = None };
  { number = 11;
    names = ["redundant-case"];
    description =
      "Redundant case in a pattern matching (unused match case).";
    since = None };
  { number = 12;
    names = ["redundant-subpat"];
    description = "Redundant sub-pattern in a pattern-matching." ;
    since = None};
  { number = 13;
    names = ["instance-variable-override"];
    description = "Instance variable overridden.";
    since = None };
  { number = 14;
    names = ["illegal-backslash"];
    description = "Illegal backslash escape in a string constant.";
    since = None };
  { number = 15;
    names = ["implicit-public-methods"];
    description = "Private method made public implicitly.";
    since = None };
  { number = 16;
    names = ["unerasable-optional-argument"];
    description = "Unerasable optional argument.";
    since = None };
  { number = 17;
    names = ["undeclared-virtual-method"];
    description = "Undeclared virtual method.";
    since = None };
  { number = 18;
    names = ["not-principal"];
    description = "Non-principal type.";
    since = None };
  { number = 19;
    names = ["non-principal-labels"];
    description = "Type without principality.";
    since = None };
  { number = 20;
    names = ["ignored-extra-argument"];
    description = "Unused function argument.";
    since = None };
  { number = 21;
    names = ["nonreturning-statement"];
    description = "Non-returning statement.";
    since = None };
  { number = 22;
    names = ["preprocessor"];
    description = "Preprocessor warning.";
    since = None };
  { number = 23;
    names = ["useless-record-with"];
    description = "Useless record \"with\" clause.";
    since = None };
  { number = 24;
    names = ["bad-module-name"];
    description =
    "Bad module name: the source file name is not a valid OCaml module name.";
    since = None };
  { number = 25;
    names = [];
    description = "Ignored: now part of warning 8.";
    since = None };
  { number = 26;
    names = ["unused-var"];
    description =
    "Suspicious unused variable: unused variable that is bound\n\
    \    with \"let\" or \"as\", and doesn't start with an underscore (\"_\")\n\
    \    character.";
    since = None };
  { number = 27;
    names = ["unused-var-strict"];
    description =
    "Innocuous unused variable: unused variable that is not bound with\n\
    \    \"let\" nor \"as\", and doesn't start with an underscore (\"_\")\n\
    \    character.";
    since = None };
  { number = 28;
    names = ["wildcard-arg-to-constant-constr"];
    description =
      "Wildcard pattern given as argument to a constant constructor.";
    since = None };
  { number = 29;
    names = ["eol-in-string"];
    description =
      "Unescaped end-of-line in a string constant (non-portable code).";
    since = None };
  { number = 30;
    names = ["duplicate-definitions"];
    description =
      "Two labels or constructors of the same name are defined in two\n\
      \    mutually recursive types.";
    since = None };
  { number = 31;
    names = ["module-linked-twice"];
    description =
      "A module is linked twice in the same executable.\n\
      \    Ignored: now a hard error (since 5.1).";
    since = None };
  { number = 32;
    names = ["unused-value-declaration"];
    description = "Unused value declaration.";
    since = since 4 0 };
  { number = 33;
    names = ["unused-open"];
    description = "Unused open statement.";
    since = since 4 0 };
  { number = 34;
    names = ["unused-type-declaration"];
    description = "Unused type declaration.";
    since = since 4 0 };
  { number = 35;
    names = ["unused-for-index"];
    description = "Unused for-loop index.";
    since = since 4 0 };
  { number = 36;
    names = ["unused-ancestor"];
    description = "Unused ancestor variable.";
    since = since 4 0 };
  { number = 37;
    names = ["unused-constructor"];
    description = "Unused constructor.";
    since = since 4 0 };
  { number = 38;
    names = ["unused-extension"];
    description = "Unused extension constructor.";
    since = since 4 0 };
  { number = 39;
    names = ["unused-rec-flag"];
    description = "Unused rec flag.";
    since = since 4 0 };
  { number = 40;
    names = ["name-out-of-scope"];
    description = "Constructor or label name used out of scope.";
    since = since 4 1 };
  { number = 41;
    names = ["ambiguous-name"];
    description = "Ambiguous constructor or label name.";
    since = since 4 1 };
  { number = 42;
    names = ["disambiguated-name"];
    description =
      "Disambiguated constructor or label name (compatibility warning).";
    since = since 4 1 };
  { number = 43;
    names = ["nonoptional-label"];
    description = "Nonoptional label applied as optional.";
    since = since 4 1 };
  { number = 44;
    names = ["open-shadow-identifier"];
    description = "Open statement shadows an already defined identifier.";
    since = since 4 1 };
  { number = 45;
    names = ["open-shadow-label-constructor"];
    description =
      "Open statement shadows an already defined label or constructor.";
    since = since 4 1 };
  { number = 46;
    names = ["bad-env-variable"];
    description = "Error in environment variable.";
    since = since 4 1 };
  { number = 47;
    names = ["attribute-payload"];
    description = "Illegal attribute payload.";
    since = since 4 2 };
  { number = 48;
    names = ["eliminated-optional-arguments"];
    description = "Implicit elimination of optional arguments.";
    since = since 4 2 };
  { number = 49;
    names = ["no-cmi-file"];
    description = "Absent cmi file when looking up module alias.";
    since = since 4 2 };
  { number = 50;
    names = ["unexpected-docstring"];
    description = "Unexpected documentation comment.";
    since = since 4 3 };
  { number = 51;
    names = ["wrong-tailcall-expectation"];
    description =
      "Function call annotated with an incorrect @tailcall attribute.";
    since = since 4 3 };
  { number = 52;
    names = ["fragile-literal-pattern"];
    description = "Fragile constant pattern.";
    since = since 4 3 };
  { number = 53;
    names = ["misplaced-attribute"];
    description = "Attribute cannot appear in this context.";
    since = since 4 3 };
  { number = 54;
    names = ["duplicated-attribute"];
    description = "Attribute used more than once on an expression.";
    since = since 4 3 };
  { number = 55;
    names = ["inlining-impossible"];
    description = "Inlining impossible.";
    since = since 4 3 };
  { number = 56;
    names = ["unreachable-case"];
    description =
      "Unreachable case in a pattern-matching (based on type information).";
    since = since 4 3 };
  { number = 57;
    names = ["ambiguous-var-in-pattern-guard"];
    description = "Ambiguous or-pattern variables under guard.";
    since = since 4 3 };
  { number = 58;
    names = ["no-cmx-file"];
    description = "Missing cmx file.";
    since = since 4 3 };
  { number = 59;
    names = ["flambda-assignment-to-non-mutable-value"];
    description = "Assignment to non-mutable value.";
    since = since 4 3 };
  { number = 60;
    names = ["unused-module"];
    description = "Unused module declaration.";
    since = since 4 4 };
  { number = 61;
    names = ["unboxable-type-in-prim-decl"];
    description = "Unboxable type in primitive declaration.";
    since = since 4 4 };
  { number = 62;
    names = ["constraint-on-gadt"];
    description = "Type constraint on GADT type declaration.";
    since = since 4 6 };
  { number = 63;
    names = ["erroneous-printed-signature"];
    description = "Erroneous printed signature.";
    since = since 4 8 };
  { number = 64;
    names = ["unsafe-array-syntax-without-parsing"];
    description =
      "-unsafe used with a preprocessor returning a syntax tree.";
    since = since 4 8 };
  { number = 65;
    names = ["redefining-unit"];
    description = "Type declaration defining a new '()' constructor.";
    since = since 4 8 };
  { number = 66;
    names = ["unused-open-bang"];
    description = "Unused open! statement.";
    since = since 4 8 };
  { number = 67;
    names = ["unused-functor-parameter"];
    description = "Unused functor parameter.";
    since = since 4 10 };
  { number = 68;
    names = ["match-on-mutable-state-prevent-uncurry"];
    description =
      "Pattern-matching depending on mutable state prevents the remaining \n\
      \    arguments from being uncurried.";
    since = since 4 12 };
  { number = 69;
    names = ["unused-field"];
    description = "Unused record field.";
    since = since 4 13 };
  { number = 70;
    names = ["missing-mli"];
    description = "Missing interface file.";
    since = since 4 13 };
  { number = 71;
    names = ["unused-tmc-attribute"];
    description = "Unused @tail_mod_cons attribute.";
    since = since 4 14 };
  { number = 72;
    names = ["tmc-breaks-tailcall"];
    description = "A tail call is turned into a non-tail call \
                   by the @tail_mod_cons transformation.";
    since = since 4 14 };
  { number = 73;
    names = ["generative-application-expects-unit"];
    description = "A generative functor is applied to an empty structure \
                   (struct end) rather than to ().";
    since = since 5 1 };
  { number = 74;
    names = ["degraded-to-partial-match"];
    description = "A pattern-matching is compiled as partial \
                   even if it appears to be total.";
    since = since 5 3 };
]

let name_to_number =
  let h = Hashtbl.create last_warning_number in
  List.iter (fun {number; names; _} ->
      List.iter (fun name -> Hashtbl.add h name number) names
    ) descriptions;
  fun s -> Hashtbl.find_opt h s

(* Must be the max number returned by the [number] function. *)

let letter = function
  | 'a' ->
     let rec loop i = if i = 0 then [] else i :: loop (i - 1) in
     loop last_warning_number
  | 'b' -> []
  | 'c' -> [1; 2]
  | 'd' -> [3]
  | 'e' -> [4]
  | 'f' -> [5]
  | 'g' -> []
  | 'h' -> []
  | 'i' -> []
  | 'j' -> []
  | 'k' -> [32; 33; 34; 35; 36; 37; 38; 39]
  | 'l' -> [6]
  | 'm' -> [7]
  | 'n' -> []
  | 'o' -> []
  | 'p' -> [8]
  | 'q' -> []
  | 'r' -> [9]
  | 's' -> [10]
  | 't' -> []
  | 'u' -> [11; 12]
  | 'v' -> [13]
  | 'w' -> []
  | 'x' -> [14; 15; 16; 17; 18; 19; 20; 21; 22; 23; 24; 30]
  | 'y' -> [26]
  | 'z' -> [27]
  | _ -> assert false

type state =
  {
    active: bool array;
    error: bool array;
    alerts: (Misc.Stdlib.String.Set.t * bool); (* false:set complement *)
    alert_errors: (Misc.Stdlib.String.Set.t * bool); (* false:set complement *)
  }

let current =
  ref
    {
      active = Array.make (last_warning_number + 1) true;
      error = Array.make (last_warning_number + 1) false;
      alerts = (Misc.Stdlib.String.Set.empty, false);
      alert_errors = (Misc.Stdlib.String.Set.empty, true); (* all soft *)
    }

let disabled = ref false

let without_warnings f =
  Misc.protect_refs [Misc.R(disabled, true)] f

let backup () = !current

let restore x = current := x

let is_active x =
  not !disabled && (!current).active.(number x)

let is_error x =
  not !disabled && (!current).error.(number x)

let alert_is_active {kind; _} =
  not !disabled &&
  let (set, pos) = (!current).alerts in
  Misc.Stdlib.String.Set.mem kind set = pos

let alert_is_error {kind; _} =
  not !disabled &&
  let (set, pos) = (!current).alert_errors in
  Misc.Stdlib.String.Set.mem kind set = pos

let with_state state f =
  let prev = backup () in
  restore state;
  try
    let r = f () in
    restore prev;
    r
  with exn ->
    restore prev;
    raise exn

let mk_lazy f =
  let state = backup () in
  lazy (with_state state f)

let set_alert ~error ~enable s =
  let upd =
    match s with
    | "all" ->
        (Misc.Stdlib.String.Set.empty, not enable)
    | s ->
        let (set, pos) =
          if error then (!current).alert_errors else (!current).alerts
        in
        let f =
          if enable = pos
          then Misc.Stdlib.String.Set.add
          else Misc.Stdlib.String.Set.remove
        in
        (f s set, pos)
  in
  if error then
    current := {(!current) with alert_errors=upd}
  else
    current := {(!current) with alerts=upd}

let parse_alert_option s =
  let n = String.length s in
  let id_char = function
    | 'a'..'z' | 'A'..'Z' | '_' | '\'' | '0'..'9' -> true
    | _ -> false
  in
  let rec parse_id i =
    if i < n && id_char s.[i] then parse_id (i + 1) else i
  in
  let rec scan i =
    if i = n then ()
    else if i + 1 = n then raise (Arg.Bad "Ill-formed list of alert settings")
    else match s.[i], s.[i+1] with
      | '+', '+' -> id (set_alert ~error:true ~enable:true) (i + 2)
      | '+', _ -> id (set_alert ~error:false ~enable:true) (i + 1)
      | '-', '-' -> id (set_alert ~error:true ~enable:false) (i + 2)
      | '-', _ -> id (set_alert ~error:false ~enable:false) (i + 1)
      | '@', _ ->
          id (fun s ->
              set_alert ~error:true ~enable:true s;
              set_alert ~error:false ~enable:true s)
            (i + 1)
      | _ -> raise (Arg.Bad "Ill-formed list of alert settings")
  and id f i =
    let j = parse_id i in
    if j = i then raise (Arg.Bad "Ill-formed list of alert settings");
    let id = String.sub s i (j - i) in
    f id;
    scan j
  in
  scan 0

type modifier =
  | Set (** +a *)
  | Clear (** -a *)
  | Set_all (** @a *)

type token =
  | Letter of char * modifier option
  | Num of int * int * modifier

let ghost_loc_in_file name =
  let pos = { Lexing.dummy_pos with pos_fname = name } in
  { loc_start = pos; loc_end = pos; loc_ghost = true }

let letter_alert tokens =
  let print_warning_char ppf c =
    let lowercase = Char.lowercase_ascii c = c in
    Format.fprintf ppf "%c%c"
      (if lowercase then '-' else '+') c
  in
  let print_modifier ppf = function
    | Set_all -> Format.fprintf ppf "@"
    | Clear -> Format.fprintf ppf "-"
    | Set -> Format.fprintf ppf "+"
  in
  let print_token ppf = function
    | Num (a,b,m) -> if a = b then
          Format.fprintf ppf "%a%d" print_modifier m a
        else
          Format.fprintf ppf "%a%d..%d" print_modifier m a b
    | Letter(l,Some m) -> Format.fprintf ppf "%a%c" print_modifier m l
    | Letter(l,None) -> print_warning_char ppf l
  in
  let consecutive_letters =
    (* we are tracking sequences of 2 or more consecutive unsigned letters
       in warning strings, for instance in '-w "not-principa"'. *)
    let commit_chunk l = function
      | [] | [ _ ] -> l
      | _ :: _ :: _ as chunk -> List.rev chunk :: l
    in
    let group_consecutive_letters (l,current) = function
    | Letter (x, None) -> (l, x::current)
    | _ -> (commit_chunk l current, [])
    in
    let l, on_going =
      List.fold_left group_consecutive_letters ([],[]) tokens
    in
    commit_chunk l on_going
  in
  match consecutive_letters with
  | [] -> None
  | example :: _  ->
      let nowhere = ghost_loc_in_file "_none_" in
      let spelling_hint ppf =
        let max_seq_len =
          List.fold_left (fun l x -> Int.max l (List.length x))
            0 consecutive_letters
        in
        if max_seq_len >= 5 then
          Format.fprintf ppf
            "@ @[Hint: Did you make a spelling mistake \
             when using a mnemonic name?@]"
        else
          ()
      in
      let message =
        Format.asprintf
          "@[<v>@[Setting a warning with a sequence of lowercase \
           or uppercase letters,@ like '%a',@ is deprecated.@]@ \
           @[Use the equivalent signed form:@ %t.@]@ \
           @[Hint: Enabling or disabling a warning by its mnemonic name \
           requires a + or - prefix.@]\
           %t@?@]"
          Format.(pp_print_list ~pp_sep:(fun _ -> ignore) pp_print_char) example
          (fun ppf -> List.iter (print_token ppf) tokens)
          spelling_hint
      in
      Some {
        kind="ocaml_deprecated_cli";
        use=nowhere; def=nowhere;
        message
      }


let parse_warnings s =
  let error () = raise (Arg.Bad "Ill-formed list of warnings") in
  let rec get_num n i =
    if i >= String.length s then i, n
    else match s.[i] with
    | '0'..'9' -> get_num (10 * n + Char.code s.[i] - Char.code '0') (i + 1)
    | _ -> i, n
  in
  let get_range i =
    let i, n1 = get_num 0 i in
    if i + 2 < String.length s && s.[i] = '.' && s.[i + 1] = '.' then
      let i, n2 = get_num 0 (i + 2) in
      if n2 < n1 then error ();
      i, n1, n2
    else
      i, n1, n1
  in
  let rec loop tokens i =
    if i >= String.length s then List.rev tokens else
    match s.[i] with
    | 'A' .. 'Z' | 'a' .. 'z' ->
        loop (Letter(s.[i],None)::tokens) (i+1)
    | '+' -> loop_letter_num tokens Set (i+1)
    | '-' -> loop_letter_num tokens Clear (i+1)
    | '@' -> loop_letter_num tokens Set_all (i+1)
    | _ -> error ()
  and loop_letter_num tokens modifier i =
    if i >= String.length s then error () else
    match s.[i] with
    | '0' .. '9' ->
        let i, n1, n2 = get_range i in
        loop (Num(n1,n2,modifier)::tokens) i
    | 'A' .. 'Z' | 'a' .. 'z' ->
       loop (Letter(s.[i],Some modifier)::tokens) (i+1)
    | _ -> error ()
  in
  loop [] 0

let parse_opt error active errflag s =
  let flags = if errflag then error else active in
  let action modifier i = match modifier with
    | Set ->
        if i = 3 then set_alert ~error:errflag ~enable:true "deprecated"
        else flags.(i) <- true
    | Clear ->
        if i = 3 then set_alert ~error:errflag ~enable:false "deprecated"
        else flags.(i) <- false
    | Set_all ->
        if i = 3 then begin
          set_alert ~error:false ~enable:true "deprecated";
          set_alert ~error:true ~enable:true "deprecated"
        end
        else begin
          active.(i) <- true;
          error.(i) <- true
        end
  in
  let eval = function
    | Letter(c, m) ->
        let lc = Char.lowercase_ascii c in
        let modifier = match m with
          | None -> if c = lc then Clear else Set
          | Some m -> m
        in
        List.iter (action modifier) (letter lc)
    | Num(n1,n2,modifier) ->
        for n = n1 to Int.min n2 last_warning_number do action modifier n done
  in
  let parse_and_eval s =
    let tokens = parse_warnings s in
    List.iter eval tokens;
    letter_alert tokens
  in
   match name_to_number s with
  | Some n -> action Set n; None
  | None ->
      if s = "" then parse_and_eval s
      else begin
        let rest = String.sub s 1 (String.length s - 1) in
        match s.[0], name_to_number rest with
        | '+', Some n -> action Set n; None
        | '-', Some n -> action Clear n; None
        | '@', Some n -> action Set_all n; None
        | _ -> parse_and_eval s
      end

let parse_options errflag s =
  let error = Array.copy (!current).error in
  let active = Array.copy (!current).active in
  let alerts = parse_opt error active errflag s in
  current := {(!current) with error; active};
  alerts

(* If you change these, don't forget to change them in man/ocamlc.m *)
let defaults_w = "+a-4-7-9-27-29-30-32..42-44-45-48-50-60-66..70-74"
let defaults_warn_error = "-a"
let default_disabled_alerts = [ "unstable"; "unsynchronized_access" ]

let () = ignore @@ parse_options false defaults_w
let () = ignore @@ parse_options true defaults_warn_error
let () =
  List.iter (set_alert ~error:false ~enable:false) default_disabled_alerts

let message = function
  | Comment_start ->
      "this `(*' is the start of a comment.\n\
       Hint: Did you forget spaces when writing the infix operator `( * )'?"
  | Comment_not_end -> "this is not the end of a comment."
  | Fragile_match "" ->
      "this pattern-matching is fragile."
  | Fragile_match s ->
      "this pattern-matching is fragile.\n\
       It will remain exhaustive when constructors are added to type " ^ s ^ "."
  | Ignored_partial_application ->
      "this function application is partial,\n\
       maybe some arguments are missing."
  | Labels_omitted [] -> assert false
  | Labels_omitted [l] ->
     "label " ^ l ^ " was omitted in the application of this function."
  | Labels_omitted ls ->
     "labels " ^ String.concat ", " ls ^
       " were omitted in the application of this function."
  | Method_override [lab] ->
      "the method " ^ lab ^ " is overridden."
  | Method_override (cname :: slist) ->
      String.concat " "
        ("the following methods are overridden by the class"
         :: cname  :: ":\n " :: slist)
  | Method_override [] -> assert false
  | Partial_match "" -> "this pattern-matching is not exhaustive."
  | Partial_match s ->
      "this pattern-matching is not exhaustive.\n\
       Here is an example of a case that is not matched:\n" ^ s
  | Missing_record_field_pattern s ->
      "the following labels are not bound in this record pattern:\n" ^ s ^
      "\nEither bind these labels explicitly or add '; _' to the pattern."
  | Non_unit_statement ->
      "this expression should have type unit."
  | Redundant_case -> "this match case is unused."
  | Redundant_subpat -> "this sub-pattern is unused."
  | Instance_variable_override [lab] ->
      "the instance variable " ^ lab ^ " is overridden."
  | Instance_variable_override (cname :: slist) ->
      String.concat " "
        ("the following instance variables are overridden by the class"
         :: cname  :: ":\n " :: slist)
  | Instance_variable_override [] -> assert false
  | Illegal_backslash ->
    "illegal backslash escape in string.\n\
    Hint: Single backslashes \\ are reserved for escape sequences\n\
    (\\n, \\r, ...). Did you check the list of OCaml escape sequences?\n\
    To get a backslash character, escape it with a second backslash: \\\\."
  | Implicit_public_methods l ->
      "the following private methods were made public implicitly:\n "
      ^ String.concat " " l ^ "."
  | Unerasable_optional_argument -> "this optional argument cannot be erased."
  | Undeclared_virtual_method m -> "the virtual method "^m^" is not declared."
  | Not_principal msg ->
      Format_doc.asprintf "%a is not principal."
        Format_doc.pp_doc msg
  | Non_principal_labels s -> s^" without principality."
  | Ignored_extra_argument -> "this argument will not be used by the function."
  | Nonreturning_statement ->
      "this statement never returns (or has an unsound type.)"
  | Preprocessor s -> s
  | Useless_record_with ->
      "all the fields are explicitly listed in this record:\n\
       the 'with' clause is useless."
  | Bad_module_name (modname) ->
      "bad source file name: \"" ^ modname ^ "\" is not a valid module name."
  | All_clauses_guarded ->
      "this pattern-matching is not exhaustive.\n\
       All clauses in this pattern-matching are guarded."
  | Unused_var v | Unused_var_strict v -> "unused variable " ^ v ^ "."
  | Wildcard_arg_to_constant_constr ->
     "wildcard pattern given as argument to a constant constructor"
  | Eol_in_string ->
     "unescaped end-of-line in a string constant\n\
      (non-portable behavior before OCaml 5.2)"
  | Duplicate_definitions (kind, cname, tc1, tc2) ->
      Printf.sprintf "the %s %s is defined in both types %s and %s."
        kind cname tc1 tc2
  | Unused_value_declaration v -> "unused value " ^ v ^ "."
  | Unused_open s -> "unused open " ^ s ^ "."
  | Unused_open_bang s -> "unused open! " ^ s ^ "."
  | Unused_type_declaration s -> "unused type " ^ s ^ "."
  | Unused_for_index s -> "unused for-loop index " ^ s ^ "."
  | Unused_ancestor s -> "unused ancestor variable " ^ s ^ "."
  | Unused_constructor (s, Unused) -> "unused constructor " ^ s ^ "."
  | Unused_constructor (s, Not_constructed) ->
      "constructor " ^ s ^
      " is never used to build values.\n\
        (However, this constructor appears in patterns.)"
  | Unused_constructor (s, Only_exported_private) ->
      "constructor " ^ s ^
      " is never used to build values.\n\
        Its type is exported as a private type."
  | Unused_extension (s, is_exception, complaint) ->
     let kind =
       if is_exception then "exception" else "extension constructor" in
     let name = kind ^ " " ^ s in
     begin match complaint with
       | Unused -> "unused " ^ name
       | Not_constructed ->
          name ^
          " is never used to build values.\n\
           (However, this constructor appears in patterns.)"
       | Only_exported_private ->
          name ^
          " is never used to build values.\n\
            It is exported or rebound as a private extension."
     end
  | Unused_rec_flag ->
      "unused rec flag."
  | Name_out_of_scope (ty, [nm], false) ->
      nm ^ " was selected from type " ^ ty ^
      ".\nIt is not visible in the current scope, and will not \n\
       be selected if the type becomes unknown."
  | Name_out_of_scope (_, _, false) -> assert false
  | Name_out_of_scope (ty, slist, true) ->
      "this record of type "^ ty ^" contains fields that are \n\
       not visible in the current scope: "
      ^ String.concat " " slist ^ ".\n\
       They will not be selected if the type becomes unknown."
  | Ambiguous_name ([s], tl, false, expansion) ->
      s ^ " belongs to several types: " ^ String.concat " " tl ^
      "\nThe first one was selected. Please disambiguate if this is wrong."
      ^ expansion
  | Ambiguous_name (_, _, false, _ ) -> assert false
  | Ambiguous_name (_slist, tl, true, expansion) ->
      "these field labels belong to several types: " ^
      String.concat " " tl ^
      "\nThe first one was selected. Please disambiguate if this is wrong."
      ^ expansion
  | Disambiguated_name s ->
      "this use of " ^ s ^ " relies on type-directed disambiguation,\n\
       it will not compile with OCaml 4.00 or earlier."
  | Nonoptional_label s ->
      "the label " ^ s ^ " is not optional."
  | Open_shadow_identifier (kind, s) ->
      Printf.sprintf
        "this open statement shadows the %s identifier %s (which is later used)"
        kind s
  | Open_shadow_label_constructor (kind, s) ->
      Printf.sprintf
        "this open statement shadows the %s %s (which is later used)"
        kind s
  | Bad_env_variable (var, s) ->
      Printf.sprintf "illegal environment variable %s : %s" var s
  | Attribute_payload (a, s) ->
      Printf.sprintf "illegal payload for attribute '%s'.\n%s" a s
  | Eliminated_optional_arguments sl ->
      Printf.sprintf "implicit elimination of optional argument%s %s"
        (if List.length sl = 1 then "" else "s")
        (String.concat ", " sl)
  | No_cmi_file(name, None) ->
      "no cmi file was found in path for module " ^ name
  | No_cmi_file(name, Some msg) ->
      Printf.sprintf
        "no valid cmi file was found in path for module %s. %s"
        name msg
  | Unexpected_docstring unattached ->
      if unattached then "unattached documentation comment (ignored)"
      else "ambiguous documentation comment"
  | Wrong_tailcall_expectation b ->
      Printf.sprintf "expected %s"
        (if b then "tailcall" else "non-tailcall")
  | Fragile_literal_pattern ->
      let[@manual.ref "ss:warn52"] ref_manual = [ 13; 5; 3 ] in
      Format.asprintf
        "Code should not depend on the actual values of\n\
         this constructor's arguments. They are only for information\n\
         and may change in future versions. %a"
        (Format_doc.compat Misc.print_see_manual) ref_manual
  | Unreachable_case ->
      "this match case is unreachable.\n\
       Consider replacing it with a refutation case '<pat> -> .'"
  | Misplaced_attribute attr_name ->
      Printf.sprintf "the %S attribute cannot appear in this context" attr_name
  | Duplicated_attribute attr_name ->
      Printf.sprintf "the %S attribute is used more than once on this \
          expression"
        attr_name
  | Inlining_impossible reason ->
      Printf.sprintf "Cannot inline: %s" reason
  | Ambiguous_var_in_pattern_guard vars ->
      let[@manual.ref "ss:warn57"] ref_manual = [ 13; 5; 4 ] in
      let vars = List.sort String.compare vars in
      let vars_explanation =
        let in_different_places =
          "in different places in different or-pattern alternatives"
        in
        match vars with
        | [] -> assert false
        | [x] -> "variable " ^ x ^ " appears " ^ in_different_places
        | _::_ ->
            let vars = String.concat ", " vars in
            "variables " ^ vars ^ " appear " ^ in_different_places
      in
      Format.asprintf
        "Ambiguous or-pattern variables under guard;\n\
         %s.\n\
         Only the first match will be used to evaluate the guard expression.\n\
         %a"
        vars_explanation (Format_doc.compat Misc.print_see_manual) ref_manual
  | No_cmx_file name ->
      Printf.sprintf
        "no cmx file was found in path for module %s, \
         and its interface was not compiled with -opaque" name
  | Flambda_assignment_to_non_mutable_value ->
      "A potential assignment to a non-mutable value was detected \n\
        in this source file.  Such assignments may generate incorrect code \n\
        when using Flambda."
  | Unused_module s -> "unused module " ^ s ^ "."
  | Unboxable_type_in_prim_decl t ->
      Printf.sprintf
        "This primitive declaration uses type %s, whose representation\n\
         may be either boxed or unboxed. Without an annotation to indicate\n\
         which representation is intended, the boxed representation has been\n\
         selected by default. This default choice may change in future\n\
         versions of the compiler, breaking the primitive implementation.\n\
         You should explicitly annotate the declaration of %s\n\
         with [@@boxed] or [@@unboxed], so that its external interface\n\
         remains stable in the future." t t
  | Constraint_on_gadt ->
      "Type constraints do not apply to GADT cases of variant types."
  | Erroneous_printed_signature s ->
      "The printed interface differs from the inferred interface.\n\
       The inferred interface contained items which could not be printed\n\
       properly due to name collisions between identifiers.\n"
     ^ s
     ^ "\nBeware that this warning is purely informational and will not catch\n\
        all instances of erroneous printed interface."
  | Unsafe_array_syntax_without_parsing ->
     "option -unsafe used with a preprocessor returning a syntax tree"
  | Redefining_unit name ->
      Printf.sprintf
        "This type declaration is defining a new '()' constructor\n\
         which shadows the existing one.\n\
         Hint: Did you mean 'type %s = unit'?" name
  | Unused_functor_parameter s -> "unused functor parameter " ^ s ^ "."
  | Match_on_mutable_state_prevent_uncurry ->
    "This pattern depends on mutable state.\n\
     It prevents the remaining arguments from being uncurried, which will \
     cause additional closure allocations."
  | Unused_field (s, Unused) -> "unused record field " ^ s ^ "."
  | Unused_field (s, Not_read) ->
      "record field " ^ s ^
      " is never read.\n\
        (However, this field is used to build or mutate values.)"
  | Unused_field (s, Not_mutated) ->
      "mutable record field " ^ s ^
      " is never mutated."
  | Missing_mli ->
    "Cannot find interface file."
  | Unused_tmc_attribute ->
      "This function is marked @tail_mod_cons\n\
       but is never applied in TMC position."
  | Tmc_breaks_tailcall ->
      "This call\n\
       is in tail-modulo-cons position in a TMC function,\n\
       but the function called is not itself specialized for TMC,\n\
       so the call will not be transformed into a tail call.\n\
       Please either mark the called function with the [@tail_mod_cons]\n\
       attribute, or mark this call with the [@tailcall false] attribute\n\
       to make its non-tailness explicit."
  | Generative_application_expects_unit ->
      "A generative functor\n\
       should be applied to '()'; using '(struct end)' is deprecated."
  | Degraded_to_partial_match ->
      let[@manual.ref "ss:warn74"] ref_manual = [ 13; 5; 5 ] in
      Format.asprintf
        "This pattern-matching is compiled \n\
         as partial, even if it appears to be total. \
         It may generate a Match_failure\n\
         exception. This typically occurs due to \
         complex matches on mutable fields.\n\
         %a"
        (Format_doc.compat Misc.print_see_manual) ref_manual
;;

let nerrors = ref 0

type reporting_information =
  { id : string
  ; message : string
  ; is_error : bool
  ; sub_locs : (loc * string) list;
  }

let id_name w =
  let n = number w in
  match List.find_opt (fun {number; _} -> number = n) descriptions with
  | Some {names = s :: _; _} ->
      Printf.sprintf "%d [%s]" n s
  | _ ->
      string_of_int n

let report w =
  match is_active w with
  | false -> `Inactive
  | true ->
     if is_error w then incr nerrors;
     `Active
       { id = id_name w;
         message = message w;
         is_error = is_error w;
         sub_locs = [];
       }

let report_alert (alert : alert) =
  match alert_is_active alert with
  | false -> `Inactive
  | true ->
      let is_error = alert_is_error alert in
      if is_error then incr nerrors;
      let message = Misc.normalise_eol alert.message in
       (* Reduce \r\n to \n:
           - Prevents any \r characters being printed on Unix when processing
             Windows sources
           - Prevents \r\r\n being generated on Windows, which affects the
             testsuite
       *)
      let sub_locs =
        if not alert.def.loc_ghost && not alert.use.loc_ghost then
          [
            alert.def, "Definition";
            alert.use, "Expected signature";
          ]
        else
          []
      in
      `Active
        {
          id = alert.kind;
          message;
          is_error;
          sub_locs;
        }

exception Errors

let reset_fatal () =
  nerrors := 0

let check_fatal () =
  if !nerrors > 0 then begin
    nerrors := 0;
    raise Errors;
  end

let pp_since out release_info =
  Printf.fprintf out " (since %d.%0*d)"
    release_info.Sys.major
    (if release_info.Sys.major >= 5 then 0 else 2)
    release_info.Sys.minor

let help_warnings () =
  List.iter
    (fun {number; description; names; since} ->
       let name =
         match names with
         | s :: _ -> " [" ^ s ^ "]"
         | [] -> ""
       in
       Printf.printf "%3i%s %s%a\n"
         number name description (fun out -> Option.iter (pp_since out)) since)
    descriptions;
  print_endline "  A all warnings";
  for i = Char.code 'b' to Char.code 'z' do
    let c = Char.chr i in
    match letter c with
    | [] -> ()
    | [n] ->
        Printf.printf "  %c Alias for warning %i.\n" (Char.uppercase_ascii c) n
    | l ->
        Printf.printf "  %c warnings %s.\n"
          (Char.uppercase_ascii c)
          (String.concat ", " (List.map Int.to_string l))
  done;
  exit 0
