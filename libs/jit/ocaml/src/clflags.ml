(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Command-line parameters *)

module Int_arg_helper = Arg_helper.Make (struct
  module Key = struct
    include Numbers.Int
    let of_string = int_of_string
  end

  module Value = struct
    include Numbers.Int
    let of_string = int_of_string
  end
end)
module Float_arg_helper = Arg_helper.Make (struct
  module Key = struct
    include Numbers.Int
    let of_string = int_of_string
  end

  module Value = struct
    include Numbers.Float
    let of_string = float_of_string
  end
end)

let objfiles = ref ([] : string list)   (* .cmo and .cma files *)
and ccobjs = ref ([] : string list)     (* .o, .a, .so and -cclib -lxxx *)
and dllibs = ref ([] : string list)     (* .so and -dllib -lxxx *)

let cmi_file = ref None

let compile_only = ref false            (* -c *)
and output_name = ref (None : string option) (* -o *)
and include_dirs = ref ([] : string list) (* -I *)
and hidden_include_dirs = ref ([] : string list) (* -H *)
and no_std_include = ref false          (* -nostdlib *)
and no_cwd = ref false                  (* -nocwd *)
and print_types = ref false             (* -i *)
and make_archive = ref false            (* -a *)
and debug = ref false                   (* -g *)
and debug_full = ref false              (* For full DWARF support *)
and unsafe = ref false                  (* -unsafe *)
and use_linscan = ref false             (* -linscan *)
and link_everything = ref false         (* -linkall *)
and custom_runtime = ref false          (* -custom *)
and no_check_prims = ref false          (* -no-check-prims *)
and bytecode_compatible_32 = ref false  (* -compat-32 *)
and output_c_object = ref false         (* -output-obj *)
and output_complete_object = ref false  (* -output-complete-obj *)
and output_complete_executable = ref false  (* -output-complete-exe *)
and all_ccopts = ref ([] : string list)     (* -ccopt *)
and classic = ref false                 (* -nolabels *)
and nopervasives = ref false            (* -nopervasives *)
and match_context_rows = ref 32         (* -match-context-rows *)
and safer_matching = ref false          (* -safer-matching *)
and preprocessor = ref(None : string option) (* -pp *)
and all_ppx = ref ([] : string list)        (* -ppx *)
let absname = ref false                 (* -absname *)
let annotations = ref false             (* -annot *)
let binary_annotations = ref false      (* -bin-annot *)
let store_occurrences = ref false       (* -bin-annot-occurrences *)
and use_threads = ref false             (* -thread *)
and noassert = ref false                (* -noassert *)
and verbose = ref false                 (* -verbose *)
and noversion = ref false               (* -no-version *)
and noprompt = ref false                (* -noprompt *)
and nopromptcont = ref false            (* -nopromptcont *)
and init_file = ref (None : string option)   (* -init *)
and noinit = ref false                  (* -noinit *)
and open_modules = ref []               (* -open *)
and use_prims = ref ""                  (* -use-prims ... *)
and use_runtime = ref ""                (* -use-runtime ... *)
and plugin = ref false                  (* -plugin ... *)
and principal = ref false               (* -principal *)
and real_paths = ref true               (* -short-paths *)
and recursive_types = ref false         (* -rectypes *)
and strict_sequence = ref false         (* -strict-sequence *)
and strict_formats = ref true           (* -strict-formats *)
and applicative_functors = ref true     (* -no-app-funct *)
and make_runtime = ref false            (* -make-runtime *)
and c_compiler = ref (None: string option) (* -cc *)
and no_auto_link = ref false            (* -noautolink *)
and dllpaths = ref ([] : string list)   (* -dllpath *)
and make_package = ref false            (* -pack *)
and for_package = ref (None: string option) (* -for-pack *)
and error_size = ref 500                (* -error-size *)
and float_const_prop = ref true         (* -no-float-const-prop *)
and transparent_modules = ref false     (* -trans-mod *)
let unique_ids = ref true               (* -d(no-)unique-ds *)
let locations = ref true                (* -d(no-)locations *)
let dump_source = ref false             (* -dsource *)
let dump_parsetree = ref false          (* -dparsetree *)
and dump_typedtree = ref false          (* -dtypedtree *)
and dump_shape = ref false              (* -dshape *)
and dump_rawlambda = ref false          (* -drawlambda *)
and dump_lambda = ref false             (* -dlambda *)
and dump_rawclambda = ref false         (* -drawclambda *)
and dump_clambda = ref false            (* -dclambda *)
and dump_rawflambda = ref false            (* -drawflambda *)
and dump_flambda = ref false            (* -dflambda *)
and dump_flambda_let = ref (None : int option) (* -dflambda-let=... *)
and dump_flambda_verbose = ref false    (* -dflambda-verbose *)
and dump_instr = ref false              (* -dinstr *)
and keep_camlprimc_file = ref false     (* -dcamlprimc *)

let keyword_edition: string option ref = ref None

let keep_asm_file = ref false           (* -S *)
let optimize_for_speed = ref true       (* -compact *)
and opaque = ref false                  (* -opaque *)

and dump_cmm = ref false                (* -dcmm *)
let dump_selection = ref false          (* -dsel *)
let dump_cse = ref false                (* -dcse *)
let dump_live = ref false               (* -dlive *)
let dump_spill = ref false              (* -dspill *)
let dump_split = ref false              (* -dsplit *)
let dump_interf = ref false             (* -dinterf *)
let dump_prefer = ref false             (* -dprefer *)
let dump_regalloc = ref false           (* -dalloc *)
let dump_reload = ref false             (* -dreload *)
let dump_scheduling = ref false         (* -dscheduling *)
let dump_linear = ref false             (* -dlinear *)
let dump_interval = ref false           (* -dinterval *)
let keep_startup_file = ref false       (* -dstartup *)
let dump_combine = ref false            (* -dcombine *)
let profile_columns : Profile.column list ref = ref [] (* -dprofile/-dtimings *)

let native_code = ref false             (* set to true under ocamlopt *)

let force_slash = ref false             (* for ocamldep *)
let clambda_checks = ref false          (* -clambda-checks *)
let cmm_invariants =
  ref Config.with_cmm_invariants        (* -dcmm-invariants *)

let flambda_invariant_checks =
  ref Config.with_flambda_invariants    (* -flambda-(no-)invariants *)

let dont_write_files = ref false        (* set to true under ocamldoc *)

let insn_sched_default = true
let insn_sched = ref insn_sched_default (* -[no-]insn-sched *)

let std_include_flag prefix =
  if !no_std_include then ""
  else (prefix ^ (Filename.quote Config.standard_library))

let std_include_dir () =
  if !no_std_include then [] else [Config.standard_library]

let shared = ref false (* -shared *)
let dlcode = ref true (* not -nodynlink *)

let pic_code = ref (match Config.architecture with (* -fPIC *)
                     | "amd64" | "s390x" -> true
                     | _                 -> false)

let runtime_variant = ref ""

let with_runtime = ref true         (* -with-runtime *)

let keep_docs = ref false              (* -keep-docs *)
let keep_locs = ref true               (* -keep-locs *)

let classic_inlining = ref false       (* -Oclassic *)
let inlining_report = ref false    (* -inlining-report *)

let afl_instrument = ref Config.afl_instrument (* -afl-instrument *)
let afl_inst_ratio = ref 100           (* -afl-inst-ratio *)

let function_sections = ref false      (* -function-sections *)

let simplify_rounds = ref None        (* -rounds *)
let default_simplify_rounds = ref 1        (* -rounds *)
let rounds () =
  match !simplify_rounds with
  | None -> !default_simplify_rounds
  | Some r -> r

let default_inline_threshold = if Config.flambda then 10. else 10. /. 8.
let inline_toplevel_multiplier = 16
let default_inline_toplevel_threshold =
  int_of_float ((float inline_toplevel_multiplier) *. default_inline_threshold)
let default_inline_call_cost = 5
let default_inline_alloc_cost = 7
let default_inline_prim_cost = 3
let default_inline_branch_cost = 5
let default_inline_indirect_cost = 4
let default_inline_branch_factor = 0.1
let default_inline_lifting_benefit = 1300
let default_inline_max_unroll = 0
let default_inline_max_depth = 1

let inline_threshold = ref (Float_arg_helper.default default_inline_threshold)
let inline_toplevel_threshold =
  ref (Int_arg_helper.default default_inline_toplevel_threshold)
let inline_call_cost = ref (Int_arg_helper.default default_inline_call_cost)
let inline_alloc_cost = ref (Int_arg_helper.default default_inline_alloc_cost)
let inline_prim_cost = ref (Int_arg_helper.default default_inline_prim_cost)
let inline_branch_cost =
  ref (Int_arg_helper.default default_inline_branch_cost)
let inline_indirect_cost =
  ref (Int_arg_helper.default default_inline_indirect_cost)
let inline_branch_factor =
  ref (Float_arg_helper.default default_inline_branch_factor)
let inline_lifting_benefit =
  ref (Int_arg_helper.default default_inline_lifting_benefit)
let inline_max_unroll =
  ref (Int_arg_helper.default default_inline_max_unroll)
let inline_max_depth =
  ref (Int_arg_helper.default default_inline_max_depth)


let unbox_specialised_args = ref true   (* -no-unbox-specialised-args *)
let unbox_free_vars_of_closures = ref true
let unbox_closures = ref false          (* -unbox-closures *)
let default_unbox_closures_factor = 10
let unbox_closures_factor =
  ref default_unbox_closures_factor      (* -unbox-closures-factor *)
let remove_unused_arguments = ref false (* -remove-unused-arguments *)

type inlining_arguments = {
  inline_call_cost : int option;
  inline_alloc_cost : int option;
  inline_prim_cost : int option;
  inline_branch_cost : int option;
  inline_indirect_cost : int option;
  inline_lifting_benefit : int option;
  inline_branch_factor : float option;
  inline_max_depth : int option;
  inline_max_unroll : int option;
  inline_threshold : float option;
  inline_toplevel_threshold : int option;
}

let set_int_arg round (arg:Int_arg_helper.parsed ref) default value =
  let value : int =
    match value with
    | None -> default
    | Some value -> value
  in
  match round with
  | None ->
    arg := Int_arg_helper.set_base_default value
             (Int_arg_helper.reset_base_overrides !arg)
  | Some round ->
    arg := Int_arg_helper.add_base_override round value !arg

let set_float_arg round (arg:Float_arg_helper.parsed ref) default value =
  let value =
    match value with
    | None -> default
    | Some value -> value
  in
  match round with
  | None ->
    arg := Float_arg_helper.set_base_default value
             (Float_arg_helper.reset_base_overrides !arg)
  | Some round ->
    arg := Float_arg_helper.add_base_override round value !arg

let use_inlining_arguments_set ?round (arg:inlining_arguments) =
  let set_int = set_int_arg round in
  let set_float = set_float_arg round in
  set_int inline_call_cost default_inline_call_cost arg.inline_call_cost;
  set_int inline_alloc_cost default_inline_alloc_cost arg.inline_alloc_cost;
  set_int inline_prim_cost default_inline_prim_cost arg.inline_prim_cost;
  set_int inline_branch_cost
    default_inline_branch_cost arg.inline_branch_cost;
  set_int inline_indirect_cost
    default_inline_indirect_cost arg.inline_indirect_cost;
  set_int inline_lifting_benefit
    default_inline_lifting_benefit arg.inline_lifting_benefit;
  set_float inline_branch_factor
    default_inline_branch_factor arg.inline_branch_factor;
  set_int inline_max_depth
    default_inline_max_depth arg.inline_max_depth;
  set_int inline_max_unroll
    default_inline_max_unroll arg.inline_max_unroll;
  set_float inline_threshold
    default_inline_threshold arg.inline_threshold;
  set_int inline_toplevel_threshold
    default_inline_toplevel_threshold arg.inline_toplevel_threshold

(* o1 is the default *)
let o1_arguments = {
  inline_call_cost = None;
  inline_alloc_cost = None;
  inline_prim_cost = None;
  inline_branch_cost = None;
  inline_indirect_cost = None;
  inline_lifting_benefit = None;
  inline_branch_factor = None;
  inline_max_depth = None;
  inline_max_unroll = None;
  inline_threshold = None;
  inline_toplevel_threshold = None;
}

let classic_arguments = {
  inline_call_cost = None;
  inline_alloc_cost = None;
  inline_prim_cost = None;
  inline_branch_cost = None;
  inline_indirect_cost = None;
  inline_lifting_benefit = None;
  inline_branch_factor = None;
  inline_max_depth = None;
  inline_max_unroll = None;
  (* [inline_threshold] matches the current compiler's default.
     Note that this particular fraction can be expressed exactly in
     floating point. *)
  inline_threshold = Some (10. /. 8.);
  (* [inline_toplevel_threshold] is not used in classic mode. *)
  inline_toplevel_threshold = Some 1;
}

let o2_arguments = {
  inline_call_cost = Some (2 * default_inline_call_cost);
  inline_alloc_cost = Some (2 * default_inline_alloc_cost);
  inline_prim_cost = Some (2 * default_inline_prim_cost);
  inline_branch_cost = Some (2 * default_inline_branch_cost);
  inline_indirect_cost = Some (2 * default_inline_indirect_cost);
  inline_lifting_benefit = None;
  inline_branch_factor = None;
  inline_max_depth = Some 2;
  inline_max_unroll = None;
  inline_threshold = Some 25.;
  inline_toplevel_threshold = Some (25 * inline_toplevel_multiplier);
}

let o3_arguments = {
  inline_call_cost = Some (3 * default_inline_call_cost);
  inline_alloc_cost = Some (3 * default_inline_alloc_cost);
  inline_prim_cost = Some (3 * default_inline_prim_cost);
  inline_branch_cost = Some (3 * default_inline_branch_cost);
  inline_indirect_cost = Some (3 * default_inline_indirect_cost);
  inline_lifting_benefit = None;
  inline_branch_factor = Some 0.;
  inline_max_depth = Some 3;
  inline_max_unroll = Some 1;
  inline_threshold = Some 50.;
  inline_toplevel_threshold = Some (50 * inline_toplevel_multiplier);
}

let all_passes = ref []
let dumped_passes_list = ref []
let dumped_pass s =
  assert(List.mem s !all_passes);
  List.mem s !dumped_passes_list

let set_dumped_pass s enabled =
  if (List.mem s !all_passes) then begin
    let passes_without_s = List.filter ((<>) s) !dumped_passes_list in
    let dumped_passes =
      if enabled then
        s :: passes_without_s
      else
        passes_without_s
    in
    dumped_passes_list := dumped_passes
  end

let dump_into_file = ref false (* -dump-into-file *)
let dump_dir: string option ref = ref None (* -dump-dir *)

type 'a env_reader = {
  parse : string -> 'a option;
  print : 'a -> string;
  usage : string;
  env_var : string;
}

let color = ref None (* -color *)

let color_reader = {
  parse = (function
    | "auto" -> Some Misc.Color.Auto
    | "always" -> Some Misc.Color.Always
    | "never" -> Some Misc.Color.Never
    | _ -> None);
  print = (function
    | Misc.Color.Auto -> "auto"
    | Misc.Color.Always -> "always"
    | Misc.Color.Never -> "never");
  usage = "expected \"auto\", \"always\" or \"never\"";
  env_var = "OCAML_COLOR";
}

let error_style = ref None (* -error-style *)

let error_style_reader = {
  parse = (function
    | "contextual" -> Some Misc.Error_style.Contextual
    | "short" -> Some Misc.Error_style.Short
    | _ -> None);
  print = (function
    | Misc.Error_style.Contextual -> "contextual"
    | Misc.Error_style.Short -> "short");
  usage = "expected \"contextual\" or \"short\"";
  env_var = "OCAML_ERROR_STYLE";
}

let unboxed_types = ref false

(* This is used by the -save-ir-after option. *)
module Compiler_ir = struct
  type t = Linear

  let all = [
    Linear;
  ]

  let extension t =
    let ext =
    match t with
      | Linear -> "linear"
    in
    ".cmir-" ^ ext

  (** [extract_extension_with_pass filename] returns the IR whose extension
      is a prefix of the extension of [filename], and the suffix,
      which can be used to distinguish different passes on the same IR.
      For example, [extract_extension_with_pass "foo.cmir-linear123"]
      returns [Some (Linear, "123")]. *)
  let extract_extension_with_pass filename =
    let ext = Filename.extension filename in
    let ext_len = String.length ext in
    if ext_len <= 0 then None
    else begin
      let is_prefix ir =
        let s = extension ir in
        let s_len = String.length s in
        s_len <= ext_len && s = String.sub ext 0 s_len
      in
      let drop_prefix ir =
        let s = extension ir in
        let s_len = String.length s in
        String.sub ext s_len (ext_len - s_len)
      in
      let ir = List.find_opt is_prefix all in
      match ir with
      | None -> None
      | Some ir -> Some (ir, drop_prefix ir)
    end
end

(* This is used by the -stop-after option. *)
module Compiler_pass = struct
  (* If you add a new pass, the following must be updated:
     - the variable `passes` below
     - the manpages in man/ocaml{c,opt}.m
     - the manual manual/src/cmds/unified-options.etex
  *)
  type t = Parsing | Typing | Lambda | Scheduling | Emit

  let to_string = function
    | Parsing -> "parsing"
    | Typing -> "typing"
    | Lambda -> "lambda"
    | Scheduling -> "scheduling"
    | Emit -> "emit"

  let of_string = function
    | "parsing" -> Some Parsing
    | "typing" -> Some Typing
    | "lambda" -> Some Lambda
    | "scheduling" -> Some Scheduling
    | "emit" -> Some Emit
    | _ -> None

  let rank = function
    | Parsing -> 0
    | Typing -> 1
    | Lambda -> 2
    | Scheduling -> 50
    | Emit -> 60

  let passes = [
    Parsing;
    Typing;
    Lambda;
    Scheduling;
    Emit;
  ]
  let is_compilation_pass _ = true
  let is_native_only = function
    | Scheduling -> true
    | Emit -> true
    | _ -> false

  let enabled is_native t = not (is_native_only t) || is_native
  let can_save_ir_after = function
    | Scheduling -> true
    | _ -> false

  let available_pass_names ~filter ~native =
    passes
    |> List.filter (enabled native)
    |> List.filter filter
    |> List.map to_string

  let compare a b =
    compare (rank a) (rank b)

  let to_output_filename t ~prefix =
    match t with
    | Scheduling -> prefix ^ Compiler_ir.(extension Linear)
    | _ -> Misc.fatal_error "Not supported"

  let of_input_filename name =
    match Compiler_ir.extract_extension_with_pass name with
    | Some (Linear, _) -> Some Emit
    | None -> None
end

let stop_after = ref None (* -stop-after *)

let should_stop_after pass =
  if Compiler_pass.(rank Typing <= rank pass) && !print_types then true
  else
    match !stop_after with
    | None -> false
    | Some stop -> Compiler_pass.rank stop <= Compiler_pass.rank pass

let save_ir_after = ref []

let should_save_ir_after pass =
  List.mem pass !save_ir_after

let set_save_ir_after pass enabled =
  let other_passes = List.filter ((<>) pass) !save_ir_after in
  let new_passes =
    if enabled then
      pass :: other_passes
    else
      other_passes
  in
  save_ir_after := new_passes

let parse_keyword_edition s =
  let parse_version s =
  let bad_version () =
    raise (Arg.Bad "Ill-formed version in keywords flag,\n\
                    the supported format is <major>.<minor>, for example 5.2 .")
  in
  if s = "" then None else match String.split_on_char '.' s with
  | [] | [_] | _ :: _ :: _ :: _ -> bad_version ()
  | [major;minor] -> match int_of_string_opt major, int_of_string_opt minor with
    | Some major, Some minor -> Some (major,minor)
    | _ -> bad_version ()
  in
  match String.split_on_char '+' s with
  | [] -> None, []
  | [s] -> parse_version s, []
  | v :: rest -> parse_version v, rest

module String = Misc.Stdlib.String

let arg_spec = ref []
let arg_names = ref String.Map.empty

let reset_arguments () =
  arg_spec := [];
  arg_names := String.Map.empty

let add_arguments loc args =
  List.iter (function (arg_name, _, _) as arg ->
    try
      let loc2 = String.Map.find arg_name !arg_names in
      Printf.eprintf
        "Warning: compiler argument %s is already defined:\n" arg_name;
      Printf.eprintf "   First definition: %s\n" loc2;
      Printf.eprintf "   New definition: %s\n" loc;
    with Not_found ->
      arg_spec := !arg_spec @ [ arg ];
      arg_names := String.Map.add arg_name loc !arg_names
  ) args

let create_usage_msg program =
  Printf.sprintf "Usage: %s <options> <files>\n\
    Try '%s --help' for more information." program program


let print_arguments program =
  Arg.usage !arg_spec (create_usage_msg program)
