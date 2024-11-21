open Ast
open Type
open Error
open Common
open Globals
open CppStrings
open CppExprUtils
open CppTypeUtils
open CppAst
open CppAstTools
open CppSourceWriter
open CppContext

type tinject = {
  inj_prologue : bool -> unit;
  inj_setvar : string;
  inj_tail : string;
}

let cpp_type_of = CppRetyper.cpp_type_of
let cpp_type_of_null = CppRetyper.cpp_type_of_null
let cpp_instance_type = CppRetyper.cpp_instance_type
let type_to_string haxe_type = tcpp_to_string (cpp_type_of haxe_type)

let type_cant_be_null haxe_type =
  match cpp_type_of haxe_type with TCppScalar _ -> true | _ -> false

let type_arg_to_string name default_val arg_type prefix =
  let remap_name = keyword_remap name in
  let type_str = type_to_string arg_type in
  match default_val with
  | Some { eexpr = TConst TNull } -> (type_str, remap_name)
  | Some constant when type_cant_be_null arg_type ->
      ("::hx::Null< " ^ type_str ^ " > ", prefix ^ remap_name)
  | Some constant -> (type_str, prefix ^ remap_name)
  | _ -> (type_str, remap_name)

let cpp_var_name_of var =
  match get_meta_string var.v_meta Meta.Native with
  | Some n -> n
  | None -> keyword_remap var.v_name

let cpp_var_debug_name_of v =
  match get_meta_string v.v_meta Meta.RealPath with
  | Some n -> n
  | None -> v.v_name

(* Generate prototype text, including allowing default values to be null *)
let print_arg name default_val arg_type prefix =
  let n, t = type_arg_to_string name default_val arg_type prefix in
  n ^ " " ^ t

(* Generate prototype text, including allowing default values to be null *)
let print_arg_name name default_val arg_type prefix =
  let n, _ = type_arg_to_string name default_val arg_type prefix in
  n

let print_arg_list arg_list prefix =
  String.concat ","
    (List.map (fun (v, o) -> print_arg v.v_name o v.v_type prefix) arg_list)

let print_arg_list_name arg_list prefix =
  String.concat ","
    (List.map
       (fun (v, o) -> print_arg_name v.v_name o v.v_type prefix)
       arg_list)

let print_arg_names args =
  String.concat "," (List.map (fun (name, _, _) -> keyword_remap name) args)

let rec print_tfun_arg_list include_names arg_list =
  let oType o arg_type =
    let type_str = type_to_string arg_type in
    (* type_str may have already converted Null<X> to Dynamic because of NotNull tag ... *)
    if o && type_cant_be_null arg_type && type_str <> "Dynamic" then
      "::hx::Null< " ^ type_str ^ " > "
    else type_str
  in
  match arg_list with
  | [] -> ""
  | [ (name, o, arg_type) ] ->
      oType o arg_type ^ if include_names then " " ^ keyword_remap name else ""
  | (name, o, arg_type) :: remaining ->
      oType o arg_type
      ^ (if include_names then " " ^ keyword_remap name else "")
      ^ ","
      ^ print_tfun_arg_list include_names remaining

let has_new_gc_references class_def =
  let is_gc_reference field =
    should_implement_field field
    && is_data_member field
    && not (type_cant_be_null field.cf_type)
  in
  List.exists is_gc_reference class_def.cl_ordered_fields

let rec has_gc_references class_def =
  (match class_def.cl_super with
  | Some def when has_gc_references (fst def) -> true
  | _ -> false)
  || has_new_gc_references class_def

let rec find_next_super_iteration class_def =
  match class_def.cl_super with
  | Some (klass, params) when has_new_gc_references klass ->
      tcpp_to_string_suffix "_obj" (cpp_instance_type klass params)
  | Some (klass, _) -> find_next_super_iteration klass
  | _ -> ""

let cpp_member_name_of member =
  match get_meta_string member.cf_meta Meta.Native with
  | Some n -> n
  | None -> keyword_remap member.cf_name

let function_signature include_names tfun abi =
  match follow tfun with
  | TFun (args, ret) ->
      type_to_string ret ^ " " ^ abi ^ "("
      ^ print_tfun_arg_list include_names args
      ^ ")"
  | _ -> "void *"

let cpp_no_debug_synbol ctx var =
  ctx.ctx_debug_level <= 1
  || (match var.v_kind with VUser _ -> false | _ -> true)
  ||
  match cpp_type_of var.v_type with
  | TCppStar _ | TCppReference _ -> true
  | TCppInst (class_def, _) when Meta.has Meta.StructAccess class_def.cl_meta ->
      true
  | TCppInst (class_def, _) when Meta.has Meta.Unreflective class_def.cl_meta ->
      true
  | _ ->
      let name = cpp_var_debug_name_of var in
      String.length name > 4 && String.sub name 0 4 = "_hx_"

let cpp_debug_name_of var = keyword_remap var.v_name
let cpp_debug_var_visible ctx var = not (cpp_no_debug_synbol ctx (fst var))
let cpp_var_type_of var = tcpp_to_string (cpp_type_of var.v_type)

let mk_injection prologue set_var tail =
  Some { inj_prologue = prologue; inj_setvar = set_var; inj_tail = tail }

let tvar_arg_to_string tvar default_val prefix =
  let remap_name = cpp_var_name_of tvar in
  let type_str = cpp_var_type_of tvar in
  match default_val with
  | Some { eexpr = TConst TNull } ->
      (tcpp_to_string (cpp_type_of_null tvar.v_type), remap_name)
  | Some constant ->
      (tcpp_to_string (cpp_type_of_null tvar.v_type), prefix ^ remap_name)
  | _ -> (type_str, remap_name)

(* Generate prototype text, including allowing default values to be null *)
let cpp_arg_string tvar default_val prefix =
  let t, n = tvar_arg_to_string tvar default_val prefix in
  t ^ " " ^ n

let cpp_arg_list args prefix =
  String.concat "," (List.map (fun (v, o) -> cpp_arg_string v o prefix) args)

let gen_type ctx haxe_type = ctx.ctx_output (type_to_string haxe_type)

let cpp_macro_var_type_of var =
  let t = tcpp_to_string (cpp_type_of var.v_type) in
  if String.contains t ',' then
    Str.global_replace (Str.regexp ",") " HX_COMMA " t
  else t

let cpp_class_name klass =
  let globalNamespace =
    match get_meta_string klass.cl_meta Meta.Native with
    | Some _ -> ""
    | None -> "::"
  in
  let path = globalNamespace ^ join_class_path_remap klass.cl_path "::" in
  if is_native_class klass || path = "::String" then path else path ^ "_obj"

let rec implements_native_interface class_def =
  List.exists
    (fun (intf_def, _) ->
      is_native_gen_class intf_def || implements_native_interface intf_def)
    class_def.cl_implements
  ||
  match class_def.cl_super with
  | Some (i, _) -> implements_native_interface i
  | _ -> false

let can_quick_alloc klass =
  (not (is_native_class klass)) && not (implements_native_interface klass)

let only_stack_access haxe_type =
  match cpp_type_of haxe_type with
  | TCppInst (klass, _) -> Meta.has Meta.StackOnly klass.cl_meta
  | _ -> false

let cpp_is_static_extension member =
  Meta.has Meta.NativeStaticExtension member.cf_meta

let cpp_enum_name_of field =
  match get_meta_string field.ef_meta Meta.Native with
  | Some n -> n
  | None -> keyword_remap field.ef_name

let string_of_path path = "::" ^ join_class_path_remap path "::" ^ "_obj"

let default_value_string ctx value =
  match value.eexpr with
  | TConst (TInt i) -> Printf.sprintf "%ld" i
  | TConst (TFloat float_as_string) ->
      "((Float)" ^ Texpr.replace_separators float_as_string "" ^ ")"
  | TConst (TString s) -> strq ctx s
  | TConst (TBool b) -> if b then "true" else "false"
  | TConst TNull -> "null()"
  | TField (_, FEnum (enum, field)) ->
      string_of_path enum.e_path ^ "::" ^ cpp_enum_name_of field ^ "_dyn()"
  | _ -> "/* Hmmm " ^ s_expr_kind value ^ " */"

let cpp_gen_default_values ctx args prefix =
  List.iter
    (fun (tvar, o) ->
      let vtype = cpp_type_of tvar.v_type in
      let not_null =
        type_has_meta_key Meta.NotNull tvar.v_type || is_cpp_scalar vtype
      in
      match o with
      | Some { eexpr = TConst TNull } -> ()
      | Some const ->
          let name = cpp_var_name_of tvar in
          let spacer =
            if ctx.ctx_debug_level > 0 then "            \t" else ""
          in
          let pname = prefix ^ name in
          ctx.ctx_output
            (spacer ^ "\t" ^ tcpp_to_string vtype ^ " " ^ name ^ " = " ^ pname);
          ctx.ctx_output
            (if not_null then
               ".Default(" ^ default_value_string ctx.ctx_common const ^ ");\n"
             else
               ";\n" ^ spacer ^ "\tif (::hx::IsNull(" ^ pname ^ ")) " ^ name
               ^ " = "
               ^ default_value_string ctx.ctx_common const
               ^ ";\n")
      | _ -> ())
    args

let ctx_default_values ctx args prefix = cpp_gen_default_values ctx args prefix

let cpp_class_hash interface =
  gen_hash 0 (join_class_path interface.cl_path "::")

let cpp_template_param path native =
  let path = "::" ^ join_class_path_remap path "::" in
  if native then path
  else
    match path with
    | "::Array" -> "::hx::ArrayBase"
    | "::Int" -> "int"
    | "::Bool" -> "bool"
    | x -> x

let rec is_constant_zero expr =
  match expr.cppexpr with
  | CppFloat x when float_of_string x = 0.0 -> true
  | CppInt i when i = Int32.zero -> true
  | CppCastScalar (expr, _) -> is_constant_zero expr
  | _ -> false

let cpp_is_const_scalar_array arrayType expressions =
  List.length expressions > 0
  &&
  match arrayType with
  | TCppScalarArray _ ->
      List.for_all
        (fun expr ->
          match expr.cppexpr with
          | CppInt _ | CppFloat _ | CppString _ | CppBool _ -> true
          | _ -> false)
        expressions
  | _ -> false

let list_num l = string_of_int (List.length l)

(* This gets the class include order correct.  In the header files, we forward declare
   the class types so the header file does not have any undefined variables.
   In the cpp files, we include all the required header files, providing the actual
   types for everything.  This way there is no problem with circular class references.
*)
let gen_forward_decl writer class_path isNative =
  let output = writer#write in
  match class_path with
  | [ "@verbatim" ], file -> writer#write (guarded_include file)
  | _ ->
      let name = fst (remap_class_path class_path) in
      output
        ((if isNative then "HX_DECLARE_NATIVE" else "HX_DECLARE_CLASS")
        ^ list_num name ^ "(");
      List.iter (fun package_part -> output (package_part ^ ",")) name;
      output (snd class_path ^ ")\n")

let format_code code = String.concat "\n" (ExtString.String.nsplit code "\r\n")

let get_meta_string_full_filename meta key =
  let rec loop = function
    | [] -> ""
    | (k, _, pos) :: _ when k = key ->
        if Filename.is_relative pos.pfile then
          Path.normalize_path (Filename.concat (Sys.getcwd ()) pos.pfile)
        else pos.pfile
    | _ :: l -> loop l
  in
  loop meta

let get_meta_string_full_dirname meta key =
  let name = get_meta_string_full_filename meta key in
  try Path.normalize_path (Filename.dirname name)
  with Invalid_argument _ -> ""

let get_code meta key =
  let code = get_meta_string meta key |> Option.default "" in
  let magic_var = "${GENCPP_SOURCE_DIRECTORY}" in
  let code =
    if ExtString.String.exists code magic_var then
      let source_directory = get_meta_string_full_dirname meta key in
      let _, code = ExtString.String.replace code magic_var source_directory in
      code
    else code
  in
  if code <> "" then format_code code ^ "\n" else code

let get_class_code class_def key =
  match class_def.cl_kind with
  | KAbstractImpl abstract_def ->
      let value = get_code abstract_def.a_meta key in
      value
  | _ -> get_code class_def.cl_meta key

let with_debug ctx metadata run =
  let old_debug = ctx.ctx_debug_level in
  let no_debug = Meta.has Meta.NoDebug metadata in
  if no_debug then ctx.ctx_debug_level <- 0;
  run no_debug;
  ctx.ctx_debug_level <- old_debug

let hx_stack_push ctx output clazz func_name pos gc_stack =
  if ctx.ctx_debug_level > 0 then (
    let stripped_file = strip_file ctx.ctx_common pos.pfile in
    let esc_file = StringHelper.s_escape stripped_file in
    ctx.ctx_file_info := PMap.add stripped_file pos.pfile !(ctx.ctx_file_info);
    let full_name =
      clazz ^ "." ^ func_name
      ^
      if clazz = "*" then
        " (" ^ esc_file ^ ":" ^ string_of_int (Lexer.get_error_line pos) ^ ")"
      else ""
    in

    let hash_class_func = gen_hash 0 (clazz ^ "." ^ func_name) in
    let hash_file = gen_hash 0 stripped_file in

    let lineName = string_of_int (Lexer.get_error_line pos) in
    incr ctx.ctx_file_id;
    let classId = hash64 (clazz ^ "." ^ stripped_file) in
    let varName = "_hx_pos_" ^ classId ^ "_" ^ lineName ^ "_" ^ func_name in
    let decl =
      varName ^ ",\"" ^ clazz ^ "\",\"" ^ func_name ^ "\"," ^ hash_class_func
      ^ ",\"" ^ full_name ^ "\",\"" ^ esc_file ^ "\"," ^ lineName ^ ","
      ^ hash_file
    in
    if ctx.ctx_is_header then
      ctx.ctx_writer#write_h_unique
        ("HX_DECLARE_STACK_FRAME" ^ "(" ^ varName ^ ")\n")
    else
      ctx.ctx_writer#write_h_unique
        ((if func_name = "new" then "HX_DEFINE_STACK_FRAME"
          else "HX_LOCAL_STACK_FRAME")
        ^ "(" ^ decl ^ ")\n");
    output
      ((if gc_stack then "HX_GC_STACKFRAME" else "HX_STACKFRAME")
      ^ "(&" ^ varName ^ ")\n"))
  else if gc_stack then output "HX_JUST_GC_STACKFRAME\n"

(* Add include to source code *)
let add_include writer class_path = writer#add_include class_path

let real_interfaces =
  List.filter (function t, pl ->
      (match (t, pl) with
      | { cl_path = [ "cpp"; "rtti" ], _ }, [] -> false
      | _ -> true))

let native_field_name_remap is_static field =
  let remap_name = keyword_remap field.cf_name in
  if not is_static then remap_name
  else
    match get_meta_string field.cf_meta Meta.Native with
    | Some nativeImpl ->
        let r = Str.regexp "^[a-zA-Z_0-9]+$" in
        if Str.string_match r remap_name 0 then "_hx_" ^ remap_name
        else "_hx_f" ^ gen_hash 0 remap_name
    | None -> remap_name

let rec is_dynamic_accessor name acc field class_def =
  acc ^ "_" ^ field.cf_name = name
  && (not (List.exists (fun f -> f.cf_name = name) class_def.cl_ordered_fields))
  &&
  match class_def.cl_super with
  | None -> true
  | Some (parent, _) -> is_dynamic_accessor name acc field parent

(* Builds inheritance tree, so header files can include parents defs.  *)
let create_super_dependencies common_ctx =
  let result = Hashtbl.create 0 in
  let real_non_native_interfaces =
    List.filter (function t, pl ->
        (match (t, pl) with
        | { cl_path = [ "cpp"; "rtti" ], _ }, [] -> false
        | _ -> not (is_native_gen_class t)))
  in
  let iterator object_def =
    match object_def with
    | TClassDecl class_def when not (has_class_flag class_def CExtern) ->
        let deps = ref [] in
        (match class_def.cl_super with
        | Some super ->
            if not (has_class_flag (fst super) CExtern) then
              deps := (fst super).cl_path :: !deps
        | _ -> ());
        List.iter
          (fun imp ->
            if not (has_class_flag (fst imp) CExtern) then
              deps := (fst imp).cl_path :: !deps)
          (real_non_native_interfaces class_def.cl_implements);
        Hashtbl.add result class_def.cl_path !deps
    | TEnumDecl enum_def when not (has_enum_flag enum_def EnExtern) ->
        Hashtbl.add result enum_def.e_path []
    | _ -> ()
  in
  List.iter iterator common_ctx.types;
  result

let can_inline_constructor baseCtx class_def super_deps constructor_deps =
  match class_def.cl_constructor with
  | Some { cf_expr = Some super_func } ->
      let is_simple = ref true in
      let rec check_simple e =
        (match e.eexpr with
        | TReturn _ -> is_simple := false
        | TArrayDecl e when List.length e > 0 -> is_simple := false
        | _ -> ());
        if !is_simple then Type.iter check_simple e
      in
      check_simple super_func;
      !is_simple
      &&
      let rec known_classes class_def so_far =
        match class_def.cl_super with
        | Some super -> known_classes (fst super) ((fst super).cl_path :: so_far)
        | _ -> so_far
      in
      let allowed = known_classes class_def [ class_def.cl_path ] in
      (* Check to see if all the types required by the constructor are already in the header *)
      (* This is quite restrictive, since most classes are forward-declared *)
      let deps, _ =
        CppReferences.find_referenced_types_flags baseCtx (TClassDecl class_def)
          "new" super_deps constructor_deps false false true
      in
      List.for_all (fun dep -> List.mem dep allowed) deps
  | _ -> true

let create_constructor_dependencies common_ctx =
  let result = Hashtbl.create 0 in
  List.iter
    (fun object_def ->
      match object_def with
      | TClassDecl class_def when not (has_class_flag class_def CExtern) -> (
          match class_def.cl_constructor with
          | Some func_def -> Hashtbl.add result class_def.cl_path func_def
          | _ -> ())
      | _ -> ())
    common_ctx.types;
  result

let begin_namespace output class_path =
  List.iter
    (fun namespace -> output ("namespace " ^ namespace ^ "{\n"))
    (List.map keyword_remap (fst class_path))

let end_namespace output class_path =
  List.iter
    (fun namespace -> output ("}" ^ " // end namespace " ^ namespace ^ "\n"))
    (fst class_path)

let begin_header_file output_h def_string nativeGen =
  output_h ("#ifndef INCLUDED_" ^ def_string ^ "\n");
  output_h ("#define INCLUDED_" ^ def_string ^ "\n\n");
  output_h "#ifndef HXCPP_H\n";
  if nativeGen then (
    output_h "#ifdef HXCPP_API_LEVEL\n";
    output_h "#include <hxcpp.h>\n";
    output_h "#else\n";
    output_h "#include <hx/Native.h>\n";
    output_h "#endif\n")
  else output_h "#include <hxcpp.h>\n";
  output_h "#endif\n\n"

let end_header_file output_h def_string =
  output_h ("\n#endif /* INCLUDED_" ^ def_string ^ " */ \n")

let cpp_tfun_signature include_names args return_type =
  let argList = print_tfun_arg_list include_names args in
  let returnType = type_to_string return_type in
  "( " ^ returnType ^ " (::hx::Object::*)(" ^ argList ^ "))"

exception FieldFound of tclass_field

let find_class_implementation class_def name interface =
  let rec find def =
    List.iter
      (fun f -> if f.cf_name = name then raise (FieldFound f))
      def.cl_ordered_fields;
    match def.cl_super with Some (def, _) -> find def | _ -> ()
  in
  try
    find class_def;
    abort
      ("Could not find implementation of " ^ name ^ " in "
      ^ join_class_path class_def.cl_path "."
      ^ " required by "
      ^ join_class_path interface.cl_path ".")
      class_def.cl_pos
  with FieldFound field -> (
    match (follow field.cf_type, field.cf_kind) with
    | _, Method MethDynamic -> ""
    | TFun (args, return_type), Method _ ->
        cpp_tfun_signature false args return_type
    | _, _ -> "")

let gen_gc_name class_path =
  let class_name_text = join_class_path class_path "." in
  const_char_star class_name_text

(* All interfaces (and sub-interfaces) implemented *)
let implementations class_def =
  let implemented_hash = Hashtbl.create 0 in
  let native_implemented = Hashtbl.create 0 in

  let cpp_interface_impl_name interface =
    "_hx_" ^ join_class_path interface.cl_path "_"
  in
  let iterator impl =
    let rec descend_interface interface =
      let intf_def = fst interface in
      let interface_name = cpp_interface_impl_name intf_def in
      let hash =
        if is_native_gen_class intf_def then native_implemented
        else implemented_hash
      in
      if not (Hashtbl.mem hash interface_name) then (
        Hashtbl.replace hash interface_name intf_def;
        List.iter descend_interface intf_def.cl_implements);
      match intf_def.cl_super with
      | Some (interface, params) -> descend_interface (interface, params)
      | _ -> ()
    in
    descend_interface impl
  in

  List.iter iterator (real_interfaces class_def.cl_implements);
  (implemented_hash, native_implemented)

let needed_interface_functions implemented_instance_fields
    native_implementations =
  let have =
    List.map (fun field -> (field.cf_name, ())) implemented_instance_fields
    |> List.to_seq |> Hashtbl.of_seq
  in
  let want = ref [] in
  Hashtbl.iter
    (fun _ intf_def ->
      List.iter
        (fun field ->
          if not (Hashtbl.mem have field.cf_name) then (
            Hashtbl.replace have field.cf_name ();
            want := field :: !want))
        intf_def.cl_ordered_fields)
    native_implementations;
  !want

let gen_cpp_ast_expression_tree ctx class_name func_name function_args
    function_type injection tree =
  let writer = ctx.ctx_writer in
  let out = ctx.ctx_output in
  let lastLine = ref (-1) in
  let tempId = ref 0 in
  let strq = strq ctx.ctx_common in

  let spacer = if ctx.ctx_debug_level > 0 then "            \t" else "" in
  let output_i value =
    out spacer;
    writer#write_i value
  in

  let output_p expr value =
    if ctx.ctx_debug_level > 0 then (
      let line = Lexer.get_error_line expr.cpppos in
      let lineName = Printf.sprintf "%4d" line in
      let macro = if line != !lastLine then "HXLINE" else "HXDLIN" in
      out (macro ^ "(" ^ lineName ^ ")\t");
      lastLine := line);
    writer#write_i value
  in

  let forInjection =
    match injection with Some inject -> inject.inj_setvar <> "" | _ -> false
  in

  let cppTree =
    CppRetyper.expression ctx TCppVoid function_args function_type tree
      forInjection
  in
  let label_name i = Printf.sprintf "_hx_goto_%i" i in
  let class_hash = gen_hash_small 0 class_name in

  let rec gen_with_injection injection expr new_line =
    (match expr.cppexpr with
    | CppBlock (exprs, closures, gc_stack) ->
        writer#begin_block;
        List.iter gen_closure closures;
        (match injection with
        | Some inject -> inject.inj_prologue gc_stack
        | _ -> ());
        let remaining = ref (List.length exprs) in
        lastLine := Lexer.get_error_line tree.epos;
        List.iter
          (fun e ->
            output_p e "";
            (if !remaining = 1 then
               match injection with
               | Some inject -> out inject.inj_setvar
               | _ -> ());
            gen e;
            decr remaining;
            writer#terminate_line)
          exprs;
        (match injection with Some inject -> out inject.inj_tail | _ -> ());
        out spacer;
        if new_line then writer#end_block else writer#end_block_line
    | CppInt i ->
        out
          (Printf.sprintf
             (if i > Int32.of_int (-1000000000) && i < Int32.of_int 1000000000
              then "%ld"
              else "(int)%ld")
             i)
    | CppFloat float_as_string -> out ("((Float)" ^ float_as_string ^ ")")
    | CppString s -> out (strq s)
    | CppBool b -> out (if b then "true" else "false")
    | CppNull -> out "null()"
    | CppNil -> out "nil"
    | CppThis ThisReal -> out "::hx::ObjectPtr<OBJ_>(this)"
    | CppThis _ -> out "__this"
    | CppSuper thiscall ->
        out
          ("::hx::ObjectPtr<super>("
          ^ (if thiscall = ThisReal then "this" else "__this.mPtr")
          ^ ")")
    | CppBreak -> out "break"
    | CppContinue -> out "continue"
    | CppGoto label -> out ("goto " ^ label_name label)
    | CppVarDecl (var, init) -> (
        let name = cpp_var_name_of var in
        (if cpp_no_debug_synbol ctx var then
           out (cpp_var_type_of var ^ " " ^ name)
         else
           let dbgName = cpp_var_debug_name_of var in
           let macro = if init = None then "HX_VAR" else "HX_VARI" in
           let varType = cpp_macro_var_type_of var in
           if name <> dbgName then
             out
               (macro ^ "_NAME( " ^ varType ^ "," ^ name ^ ",\"" ^ dbgName
              ^ "\")")
           else out (macro ^ "( " ^ varType ^ "," ^ name ^ ")"));
        match init with
        | Some init ->
            out " = ";
            gen init
        | _ -> ())
    | CppEnumIndex obj ->
        gen obj;
        if cpp_is_dynamic_type obj.cpptype then
          out ".StaticCast< ::hx::EnumBase >()";
        out "->_hx_getIndex()"
    | CppNullAccess -> out ("::hx::Throw(" ^ strq "Null access" ^ ")")
    | CppFunction (func, _) -> (
        match func with
        | FuncThis (field, _) ->
            out ("this->" ^ cpp_member_name_of field ^ "_dyn()")
        | FuncInstance (expr, inst, field) ->
            gen expr;
            out
              ((if expr.cpptype = TCppString || inst = InstStruct then "."
                else "->")
              ^ cpp_member_name_of field ^ "_dyn()")
        | FuncInterface (expr, _, field) ->
            gen expr;
            out ("->__Field(" ^ strq field.cf_name ^ ", ::hx::paccDynamic)")
        | FuncStatic (clazz, _, field) -> (
            match get_meta_string field.cf_meta Meta.Native with
            | Some n -> out n
            | None ->
                out (cpp_class_name clazz);
                out ("::" ^ cpp_member_name_of field ^ "_dyn()"))
        | FuncExpression expr -> gen expr
        | FuncExtern (name, isGlobal) ->
            if isGlobal then out " ::";
            out name
        | FuncInternal (expr, name, _) ->
            gen expr;
            out ("->__Field(" ^ strq name ^ ",::hx::paccDynamic)")
        | FuncSuper _ | FuncSuperConstruct _ ->
            abort "Can't create super closure" expr.cpppos
        | FuncNew _ -> abort "Can't create new closure" expr.cpppos
        | FuncEnumConstruct _ ->
            abort "Enum constructor outside of CppCall" expr.cpppos
        | FuncFromStaticFunction ->
            abort "Can't create cpp.Function.fromStaticFunction closure"
              expr.cpppos
        | FuncTemplate _ ->
            abort "Can't create template function closure" expr.cpppos)
    | CppCall (FuncInterface (expr, clazz, field), args)
      when not (is_native_gen_class clazz) ->
        out (cpp_class_name clazz ^ "::" ^ cpp_member_name_of field ^ "(");
        gen expr;
        List.iter
          (fun arg ->
            out ",";
            gen arg)
          args;
        out ")"
    | CppCall ((FuncStatic (_, true, field) as func), arg_list)
    | CppCall ((FuncInstance (_, InstObjC, field) as func), arg_list) ->
        out "[ ";
        (match func with
        | FuncStatic (cl, _, _) -> out (join_class_path_remap cl.cl_path "::")
        | FuncInstance (expr, _, _) -> gen expr
        | _ -> ());

        let names = ExtString.String.nsplit field.cf_name ":" in
        let field_name, arg_names =
          match names with
          | name :: args -> (name, args)
          | _ -> die "" __LOC__ (* per nsplit specs, this should never happen *)
        in
        out (" " ^ field_name);
        (try
           match (arg_list, arg_names) with
           | [], _ -> ()
           | [ single_arg ], _ ->
               out ": ";
               gen single_arg
           | first_arg :: args, arg_names ->
               out ": ";
               gen first_arg;
               List.iter2
                 (fun arg arg_name ->
                   out (" " ^ arg_name ^ ": ");
                   gen arg)
                 args arg_names
         with Invalid_argument _ ->
           (* not all arguments names are known *)
           abort
             ("The function called here with name " ^ String.concat ":" names
            ^ " does not contain the right amount of arguments' names as \
               required" ^ " by the objective-c calling / naming convention:"
            ^ " expected "
             ^ string_of_int (List.length arg_list)
             ^ " and found "
             ^ string_of_int (List.length arg_names))
             expr.cpppos);
        out " ]"
    | CppCall (FuncNew (TCppInst (klass, p)), args) when can_quick_alloc klass
      ->
        out (cpp_class_path_of klass p ^ "_obj::__alloc( HX_CTX ");
        List.iter
          (fun arg ->
            out ",";
            gen arg)
          args;
        out ")"
    | CppCall (func, args) ->
        let doCall = ref true in
        let closeCall = ref "" in
        let argsRef = ref args in
        (match func with
        | FuncThis (field, _) -> out ("this->" ^ cpp_member_name_of field)
        | FuncInstance (expr, inst, field) ->
            let operator =
              if expr.cpptype = TCppString || inst = InstStruct then "."
              else "->"
            in
            gen expr;
            out (operator ^ cpp_member_name_of field)
        | FuncInterface (expr, _, field) ->
            gen expr;
            out ("->" ^ cpp_member_name_of field)
        | FuncStatic (clazz, false, field) when cpp_is_static_extension field
          -> (
            match args with
            | fst :: remaining ->
                argsRef := remaining;
                gen fst;
                out ("->" ^ cpp_member_name_of field)
            | _ ->
                abort "Native static extensions must have at least 1 argument"
                  expr.cpppos)
        | FuncStatic (clazz, _, field) -> (
            match get_meta_string field.cf_meta Meta.Native with
            | Some rename ->
                (* This is the case if you use @:native('new foo').  c++ wil group the space undesirably *)
                if String.contains rename ' ' then (
                  out "(";
                  closeCall := ")");
                out rename
            | None ->
                out (cpp_class_name clazz);
                out ("::" ^ cpp_member_name_of field))
        | FuncTemplate (clazz, field, tpath, native) ->
            (match get_meta_string field.cf_meta Meta.Native with
            | Some rename ->
                (* This is the case if you use @:native('new foo').  c++ wil group the space undesirably *)
                if String.contains rename ' ' then (
                  out "(";
                  closeCall := ")");
                out rename
            | None ->
                out (cpp_class_name clazz);
                out ("::" ^ cpp_member_name_of field));
            out ("< " ^ cpp_template_param tpath native ^ "  >")
        | FuncFromStaticFunction ->
            abort "Unexpected FuncFromStaticFunction" expr.cpppos
        | FuncEnumConstruct (enum, field) ->
            out (string_of_path enum.e_path ^ "::" ^ cpp_enum_name_of field)
        | FuncSuperConstruct (TCppInst (klass, _)) when is_native_class klass ->
            doCall := false
        | FuncSuperConstruct _ ->
            out
              ((if not ctx.ctx_real_this_ptr then "__this->" else "")
              ^ "super::__construct")
        | FuncSuper (_, TCppInst (klass, p), field) when is_native_class klass
          ->
            out (cpp_class_path_of klass p ^ "::" ^ cpp_member_name_of field)
        | FuncSuper (this, _, field) ->
            out
              ((if this == ThisReal then "this->" else "__->")
              ^ "super::" ^ cpp_member_name_of field)
        | FuncNew newType ->
            let objName =
              match newType with
              | TCppString -> "::String"
              | TCppDynamicArray -> "::cpp::VirtualArray_obj::__new"
              | TCppObjectArray _ -> "::Array_obj< ::Dynamic>::__new"
              | TCppScalarArray value ->
                  "::Array_obj< " ^ tcpp_to_string value ^ " >::__new"
              | TCppObjC klass -> cpp_class_path_of klass [] ^ "_obj::__new"
              | TCppNativePointer klass -> "new " ^ cpp_class_path_of klass []
              | TCppInst (klass, p) when is_native_class klass ->
                  cpp_class_path_of klass p
              | TCppInst (klass, p) -> cpp_class_path_of klass p ^ "_obj::__new"
              | TCppClass -> "::hx::Class_obj::__new"
              | TCppFunction _ -> tcpp_to_string newType
              | _ ->
                  abort
                    ("Unknown 'new' target " ^ tcpp_to_string newType)
                    expr.cpppos
            in
            out objName
        | FuncInternal (func, name, join) ->
            gen func;
            out (join ^ name)
        | FuncExtern (name, isGlobal) ->
            if isGlobal then out " ::";
            out name
        | FuncExpression expr -> gen expr);
        if !doCall then (
          let sep = ref "" in
          out "(";
          List.iter
            (fun arg ->
              out !sep;
              sep := ",";
              gen arg)
            !argsRef;
          out (")" ^ !closeCall))
    | CppNewNative e ->
        out "new ";
        gen e
    | CppAddressOf e ->
        out "&(";
        gen e;
        out ")"
    | CppDereference e ->
        out "(*(";
        gen e;
        out "))"
    | CppFunctionAddress (klass, member) ->
        let signature = function_signature false member.cf_type "" in
        let name = cpp_member_name_of member in
        (*let void_cast = has_meta_key field.cf_meta Meta.Void in*)
        out ("::cpp::Function< " ^ signature ^ ">(::hx::AnyCast(");
        out ("&::" ^ join_class_path_remap klass.cl_path "::" ^ "_obj::" ^ name);
        out " ))"
    | CppExtern (name, isGlobal) ->
        if isGlobal then out " ::";
        out name
    | CppDynamicField (obj, name) ->
        gen obj;
        out ("->__Field(" ^ strq name ^ ",::hx::paccDynamic)")
    | CppArray arrayLoc -> (
        match arrayLoc with
        | ArrayTyped (arrayObj, index, _) ->
            gen arrayObj;
            out "->__get(";
            gen index;
            out ")"
        | ArrayPointer (arrayObj, index) ->
            gen arrayObj;
            out ".ptr[";
            gen index;
            out "]"
        | ArrayRawPointer (arrayObj, index) ->
            gen arrayObj;
            out "[";
            gen index;
            out "]"
        | ArrayObject (arrayObj, index, elem) ->
            let close =
              if cpp_is_dynamic_type elem then ""
              else if elem = TCppDynamicArray then (
                out (tcpp_to_string elem ^ "( ");
                ")")
              else ".StaticCast< " ^ tcpp_to_string elem ^ " >()"
            in
            gen arrayObj;
            out "->__get(";
            gen index;
            out (")" ^ close)
        | ArrayVirtual (arrayObj, index) ->
            gen arrayObj;
            out "->__get(";
            gen index;
            out ")"
        | ArrayDynamic (arrayObj, index) ->
            gen arrayObj;
            out "->__GetItem(";
            gen index;
            out ")"
        | ArrayImplements (_, arrayObj, index) ->
            gen arrayObj;
            out "->__get(";
            gen index;
            out ")")
    | CppSet (lvalue, rvalue) ->
        let close =
          if expr.cpptype = TCppVoid then ""
          else (
            out "(";
            ")")
        in
        (match lvalue with
        | CppVarRef (VarClosure var)
          when is_gc_element ctx (cpp_type_of var.v_type) ->
            out ("this->_hx_set_" ^ cpp_var_name_of var ^ "(HX_CTX, ");
            gen rvalue;
            out ")"
        | CppVarRef (VarThis (member, _))
          when is_gc_element ctx (cpp_type_of member.cf_type) ->
            out ("this->_hx_set_" ^ cpp_member_name_of member ^ "(HX_CTX, ");
            gen rvalue;
            out ")"
        | CppVarRef (VarInstance (obj, member, _, "->"))
          when is_gc_element ctx (cpp_type_of member.cf_type) ->
            gen obj;
            out ("->_hx_set_" ^ cpp_member_name_of member ^ "(HX_CTX, ");
            gen rvalue;
            out ")"
        | CppVarRef (VarInternal (obj, operator, member)) ->
            gen obj;
            out (operator ^ member)
        | CppVarRef varLoc ->
            gen_val_loc varLoc true;
            out " = ";
            gen rvalue
        | CppArrayRef arrayLoc -> (
            match arrayLoc with
            | ArrayObject (arrayObj, index, _)
              when is_gc_element ctx TCppDynamic ->
                gen arrayObj;
                out "->setCtx( HX_CTX, ";
                gen index;
                out ",";
                gen rvalue;
                out ")"
            | ArrayTyped (arrayObj, index, t) when is_gc_element ctx t ->
                gen arrayObj;
                out "->setCtx( HX_CTX, ";
                gen index;
                out ",";
                gen rvalue;
                out ")"
            | ArrayObject (arrayObj, index, _)
            | ArrayTyped (arrayObj, index, _)
            | ArrayRawPointer (arrayObj, index) ->
                gen arrayObj;
                out "[";
                gen index;
                out "] = ";
                gen rvalue
            | ArrayPointer (arrayObj, index) ->
                gen arrayObj;
                out ".ptr[";
                gen index;
                out "] = ";
                gen rvalue
            | ArrayVirtual (arrayObj, index) ->
                gen arrayObj;
                out "->set(";
                gen index;
                out ",";
                gen rvalue;
                out ")"
            | ArrayDynamic (arrayObj, index) ->
                gen arrayObj;
                out "->__SetItem(";
                gen index;
                out ",";
                gen rvalue;
                out ")"
            | ArrayImplements (_, arrayObj, index) ->
                gen arrayObj;
                out "->__set(";
                gen index;
                out ",";
                gen rvalue;
                out ")")
        | CppDynamicRef (expr, name) ->
            gen expr;
            out ("->__SetField(" ^ strq name ^ ",");
            gen rvalue;
            out ",::hx::paccDynamic)"
        | CppExternRef (name, isGlobal) ->
            if isGlobal then out " ::";
            out (name ^ " = "));
        out close
    | CppCrement (incFlag, preFlag, lvalue) ->
        let op = if incFlag == CppIncrement then "++" else "--" in
        if preFlag == Prefix then out op;
        gen_lvalue lvalue;
        if preFlag == Postfix then out op
    | CppModify (op, lvalue, rvalue) ->
        out (string_of_op_eq op expr.cpppos);
        out "(";
        gen_lvalue lvalue;
        out ",";
        gen rvalue;
        out ")"
    | CppPosition (name, line, clazz, func) ->
        out
          ("::hx::SourceInfo(" ^ strq name ^ ","
          ^ string_of_int (Int32.to_int line)
          ^ "," ^ strq clazz ^ "," ^ strq func ^ ")")
    | CppClassOf (path, native) ->
        let path = "::" ^ join_class_path_remap path "::" in
        let path =
          match path with "::Int" -> "int" | "::Bool" -> "bool" | x -> x
        in
        if native then out "null()"
        else if path = "::Array" then out "::hx::ArrayBase::__mClass"
        else out ("::hx::ClassOf< " ^ path ^ " >()")
    | CppVar loc -> gen_val_loc loc false
    | CppClosure closure ->
        out
          (" ::Dynamic(new _hx_Closure_" ^ string_of_int closure.close_id ^ "(");
        let separator = ref "" in
        (match closure.close_this with
        | Some this ->
            out (if this = ThisReal then "this" else "__this");
            separator := ","
        | _ -> ());

        Hashtbl.iter
          (fun name value ->
            out !separator;
            separator := ",";
            out (keyword_remap name))
          closure.close_undeclared;
        out "))"
    | CppObjectDecl (values, isStruct) ->
        let length = List.length values in
        let lengthStr = string_of_int length in
        if expr.cpptype != TCppVoid then out " ::Dynamic(";
        if isStruct && length > 0 && length <= 5 then (
          out
            ("::hx::AnonStruct" ^ lengthStr ^ "_obj< "
            ^ String.concat ","
                (List.map
                   (fun (_, value) -> tcpp_to_string value.cpptype)
                   values)
            ^ " >::Create(");
          let sep = ref "" in
          List.iter
            (fun (name, value) ->
              out (!sep ^ strq name ^ ",");
              sep := ",";
              gen value)
            values;
          out ")")
        else (
          out ("::hx::Anon_obj::Create(" ^ lengthStr ^ ")");
          let sorted =
            List.sort
              (fun (_, _, h0) (_, _, h1) -> Int32.compare h0 h1)
              (List.map
                 (fun (name, value) -> (name, value, gen_hash32 0 name))
                 values)
          in
          writer#push_indent;
          ExtList.List.iteri
            (fun idx (name, value, _) ->
              out ("\n" ^ spacer);
              writer#write_i
                ("->setFixed(" ^ string_of_int idx ^ "," ^ strq name ^ ",");
              gen value;
              out ")")
            sorted);
        if expr.cpptype != TCppVoid then out ")";
        writer#pop_indent
    | CppArrayDecl exprList when cpp_is_const_scalar_array expr.cpptype exprList
      ->
        let arrayType =
          match expr.cpptype with
          | TCppScalarArray value -> value
          | _ -> assert false
        in
        let typeName = tcpp_to_string arrayType in
        incr ctx.ctx_file_id;

        let id =
          "_hx_array_data_" ^ class_hash ^ "_"
          ^ string_of_int !(ctx.ctx_file_id)
        in

        let out_top = ctx.ctx_writer#write_h in
        out_top ("static const " ^ typeName ^ " " ^ id ^ "[] = {\n\t");
        List.iter
          (fun expr ->
            match expr.cppexpr with
            | CppInt i -> out_top (Printf.sprintf "(%s)%ld," typeName i)
            | CppFloat f -> out_top (f ^ ",")
            | CppString s -> out_top (strq s ^ ",")
            | CppBool b -> out_top (if b then "1," else "0,")
            | _ -> die "" __LOC__)
          exprList;
        out_top "\n};\n";
        out
          ("::Array_obj< " ^ typeName ^ " >::fromData( " ^ id ^ ","
         ^ list_num exprList ^ ")")
    | CppArrayDecl exprList ->
        let count = List.length exprList in
        let countStr = string_of_int count in
        let arrayType, close =
          match expr.cpptype with
          | TCppObjectArray _ -> ("::Array_obj< ::Dynamic>", "")
          | TCppScalarArray value ->
              ("::Array_obj< " ^ tcpp_to_string value ^ " >", "")
          | TCppDynamicArray -> ("::cpp::VirtualArray_obj", "")
          | _ -> (" ::Dynamic( ::cpp::VirtualArray_obj", ")")
        in
        out (arrayType ^ "::__new(" ^ countStr ^ ")");
        ExtList.List.iteri
          (fun idx elem ->
            out ("->init(" ^ string_of_int idx ^ ",");
            gen elem;
            out ")")
          exprList;
        out close
    | CppBinop (Ast.OpUShr, left, right) ->
        out "::hx::UShr(";
        gen left;
        out ",";
        gen right;
        out ")"
    | CppBinop (Ast.OpMod, left, right) ->
        if is_constant_zero right then (
          out "::hx::Mod(";
          gen left;
          out ",(double)( ";
          gen right;
          out " ))")
        else (
          out "::hx::Mod(";
          gen left;
          out ",";
          gen right;
          out ")")
    | CppBinop (Ast.OpDiv, left, right) when is_constant_zero right ->
        out "::hx::DivByZero(";
        gen left;
        out ")"
    | CppBinop (op, left, right) ->
        let op = string_of_op op expr.cpppos in
        out "(";
        gen left;
        out (" " ^ op ^ " ");
        gen right;
        out ")"
    | CppCompare (opName, left, right, _) ->
        out ("::hx::" ^ opName ^ "( ");
        gen left;
        out ",";
        gen right;
        out " )"
    | CppNullCompare (op, left) ->
        out ("::hx::" ^ op ^ "( ");
        gen left;
        out " )"
    | CppThrow value ->
        out "HX_STACK_DO_THROW(";
        gen value;
        out ")"
    | CppReturn None -> out "return"
    | CppReturn (Some value) ->
        out "return ";
        gen value
    | CppEnumField (enum, field) ->
        out
          (string_of_path enum.e_path ^ "::" ^ cpp_enum_name_of field ^ "_dyn()")
    | CppEnumParameter (obj, field, index) -> (
        let valueType = cpp_type_of (get_nth_type field index) in
        let baseType = enum_getter_type valueType in
        gen obj;
        if cpp_is_dynamic_type obj.cpptype then
          out ".StaticCast< ::hx::EnumBase >()";
        out ("->_hx_get" ^ baseType ^ "(" ^ string_of_int index ^ ")");
        match valueType with
        | TCppObjectArray _ | TCppScalarArray _ | TCppDynamicArray | TCppClass
        | TCppEnum _ | TCppInst _ ->
            out (".StaticCast< " ^ tcpp_to_string valueType ^ " >()")
        | _ -> ())
    | CppIntSwitch (condition, cases, defVal) ->
        out "switch((int)(";
        gen condition;
        out "))";
        writer#begin_block;
        List.iter
          (fun (values, expr) ->
            out spacer;
            writer#write_i "";
            List.iter
              (fun value ->
                out ("case (int)" ^ Printf.sprintf "%ld" value ^ ": "))
              values;
            gen expr;
            out spacer;
            writer#write_i "break;\n")
          cases;
        (match defVal with
        | Some expr ->
            output_i "default:";
            gen expr
        | _ -> ());
        out spacer;
        writer#end_block
    | CppSwitch (condition, conditionType, cases, optional_default, label) ->
        let tmp_name = "_hx_switch_" ^ string_of_int !tempId in
        incr tempId;
        out (tcpp_to_string conditionType ^ " " ^ tmp_name ^ " = ");
        gen condition;
        out ";\n";
        List.iter
          (fun (cases, expression) ->
            output_i "if ( ";
            let or_str = ref "" in
            List.iter
              (fun value ->
                out (!or_str ^ " (" ^ tmp_name ^ "==");
                gen value;
                out ")";
                or_str := " || ")
              cases;
            out " )";
            gen expression)
          cases;
        (match optional_default with
        | None -> ()
        | Some default ->
            output_i "/* default */";
            gen default);
        output_i (label_name label ^ ":")
    | CppUnop (unop, value) ->
        out
          (match unop with CppNot -> "!" | CppNeg -> "-" | CppNegBits -> "~");
        out "(";
        gen value;
        out ")"
    | CppWhile (condition, block, while_flag, loop_id) ->
        (match while_flag with
        | NormalWhile ->
            out "while(";
            gen condition;
            out ")";
            lastLine := -1;
            gen block
        | DoWhile ->
            out "do ";
            lastLine := -1;
            gen_with_injection None block false;
            out " while(";
            gen condition;
            out ");\n");
        if loop_id > -1 then output_i (label_name loop_id ^ ":")
    | CppIf (condition, block, None) ->
        out "if (";
        gen condition;
        out ") ";
        gen block
    | CppIf (condition, block, Some elze) when expr.cpptype = TCppVoid ->
        out "if (";
        gen condition;
        out ") ";
        gen block;
        output_i "else ";
        gen elze
    | CppIf (condition, block, Some elze) ->
        gen condition;
        out " ? ";
        gen block;
        out " : ";
        gen elze
    | CppFor (tvar, init, loop) ->
        let varType = cpp_var_type_of tvar in
        out
          ("for(::cpp::FastIterator_obj< " ^ varType
         ^ " > *__it = ::cpp::CreateFastIterator< " ^ varType ^ " >(");
        gen init;
        out ");  __it->hasNext(); )";
        let prologue _ =
          output_i (varType ^ " " ^ cpp_var_name_of tvar ^ " = __it->next();\n")
        in
        gen_with_injection (mk_injection prologue "" "") loop true
    | CppTry (block, catches) ->
        let prologue = function
          | _ ->
              ExtList.List.iteri
                (fun idx (v, _) ->
                  output_i
                    ("HX_STACK_CATCHABLE(" ^ cpp_macro_var_type_of v ^ ", "
                   ^ string_of_int idx ^ ");\n"))
                catches
        in
        out "try ";
        gen_with_injection
          (mk_injection prologue "" "")
          block
          (List.length catches < 0);
        if List.length catches > 0 then (
          out " catch( ::Dynamic _hx_e) ";
          writer#begin_block;

          let seen_dynamic = ref false in
          let else_str = ref "" in
          List.iter
            (fun (v, catch) ->
              let type_name = cpp_var_type_of v in
              (match cpp_type_of v.v_type with
              | TCppInterface klass ->
                  let hash = cpp_class_hash klass in
                  output_i
                    (!else_str ^ "if (::hx::TIsInterface< (int)" ^ hash
                   ^ " >(_hx_e.mPtr))")
              | TCppString ->
                  output_i
                    (!else_str
                   ^ "if (_hx_e.IsClass< ::String >() && \
                      _hx_e->toString()!=null() )")
              | _ ->
                  if type_name = "Dynamic" then (
                    seen_dynamic := true;
                    output_i !else_str)
                  else
                    output_i
                      (!else_str ^ "if (_hx_e.IsClass< " ^ type_name ^ " >() )"));

              let prologue = function
                | _ ->
                    output_i "HX_STACK_BEGIN_CATCH\n";
                    output_i
                      (type_name ^ " " ^ cpp_var_name_of v ^ " = _hx_e;\n")
              in
              gen_with_injection (mk_injection prologue "" "") catch true;
              else_str := "else ")
            catches;

          if not !seen_dynamic then (
            output_i "else {\n";
            output_i "\tHX_STACK_DO_THROW(_hx_e);\n";
            output_i "}\n");
          out spacer;
          writer#end_block)
    | CppCode (value, exprs) ->
        Codegen.interpolate_code ctx.ctx_common (format_code value) exprs out
          (fun e -> gen e)
          expr.cpppos
    | CppTCast (expr, cppType) -> (
        match cppType with
        | TCppInterface i ->
            out " ::hx::interface_check(";
            gen expr;
            out ("," ^ cpp_class_hash i ^ ")")
        | _ ->
            let toType = tcpp_to_string cppType in
            if toType = "Dynamic" then (
              out " ::Dynamic(";
              gen expr;
              out ")")
            else (
              out ("::hx::TCast< " ^ toType ^ " >::cast(");
              gen expr;
              out ")"))
    | CppCastStatic (expr, toType) ->
        let close =
          match expr.cpptype with
          | TCppDynamic -> ""
          | _ ->
              out "Dynamic( ";
              ")"
        in
        gen expr;
        out (close ^ ".StaticCast< " ^ tcpp_to_string toType ^ " >()")
    | CppCast (expr, toType) -> (
        match (expr.cppexpr, expr.cpptype, toType) with
        | CppCall (FuncInternal _, _), _, _ ->
            gen expr;
            out (".StaticCast< " ^ tcpp_to_string toType ^ " >()")
        | _, TCppObjC _, _ | _, TCppObjCBlock _, _ ->
            out ("( (" ^ tcpp_to_string toType ^ ")((id) ( ");
            gen expr;
            out ") ))"
        | _, _, TCppObjectPtr ->
            out "::hx::DynamicPtr(";
            gen expr;
            out ")"
        | _, TCppPointer (_, _), TCppStar (_, _)
        | _, TCppPointer (_, _), TCppRawPointer (_, _) ->
            out ("( (" ^ tcpp_to_string toType ^ ")( (");
            gen expr;
            out ").get_raw()) )"
        | _ ->
            out ("( (" ^ tcpp_to_string toType ^ ")(");
            gen expr;
            out ") )")
    | CppCastScalar (expr, scalar) ->
        out ("( (" ^ scalar ^ ")(");
        gen expr;
        out ") )"
    | CppCastVariant expr ->
        out " ::Dynamic(";
        gen expr;
        out ")"
    | CppCastObjC (expr, klass) ->
        let path = join_class_path_remap klass.cl_path "::" in
        let toType =
          if has_class_flag klass CInterface then "id < " ^ path ^ ">"
          else path ^ " *"
        in
        out ("( (" ^ toType ^ ") (id) (");
        gen expr;
        out ") )"
    | CppCastObjCBlock (expr, args, ret) ->
        out (tcpp_objc_block_struct args ret ^ "::create( ");
        gen expr;
        out ")"
    | CppCastProtocol (expr, klass) ->
        out (join_class_path_remap klass.cl_path "::" ^ "_obj::_hx_toProtocol( ");
        gen expr;
        out ")"
    | CppCastNative expr ->
        out "(";
        gen expr;
        out ").mPtr");
    if ctx.ctx_debug_level >= 3 then
      out
        ("/* " ^ s_tcpp expr.cppexpr ^ ":" ^ tcpp_to_string expr.cpptype ^ " */")
  and gen expr = gen_with_injection None expr true
  and gen_lvalue lvalue =
    match lvalue with
    | CppVarRef varLoc -> gen_val_loc varLoc true
    | CppArrayRef arrayLoc -> (
        match arrayLoc with
        | ArrayObject (arrayObj, index, _) ->
            out "::hx::IndexRef(";
            gen arrayObj;
            out ".mPtr,";
            gen index;
            out ")"
        | ArrayTyped (arrayObj, index, _) ->
            gen arrayObj;
            out "[";
            gen index;
            out "]"
        | ArrayPointer (arrayObj, index) ->
            gen arrayObj;
            out ".ptr[";
            gen index;
            out "]"
        | ArrayRawPointer (arrayObj, index) ->
            gen arrayObj;
            out "[";
            gen index;
            out "]"
        | ArrayVirtual (arrayObj, index) | ArrayDynamic (arrayObj, index) ->
            out "::hx::IndexRef(";
            gen arrayObj;
            out ".mPtr,";
            gen index;
            out ")"
        | ArrayImplements (_, arrayObj, index) ->
            out "::hx::__ArrayImplRef(";
            gen arrayObj;
            out ",";
            gen index;
            out ")")
    | CppExternRef (name, isGlobal) ->
        if isGlobal then out " ::";
        out name
    | CppDynamicRef (expr, name) ->
        let objPtr =
          match expr.cpptype with TCppVariant -> "getObject()" | _ -> ".mPtr"
        in
        out "::hx::FieldRef((";
        gen expr;
        out (")" ^ objPtr ^ "," ^ strq name ^ ")")
  and gen_val_loc loc lvalue =
    match loc with
    | VarClosure var -> out (cpp_var_name_of var)
    | VarLocal local -> out (cpp_var_name_of local)
    | VarStatic (clazz, objc, member) -> (
        match get_meta_string member.cf_meta Meta.Native with
        | Some n -> out n
        | None ->
            if objc then (
              out (join_class_path_remap clazz.cl_path "::");
              out ("." ^ cpp_member_name_of member))
            else (
              out (cpp_class_name clazz);
              out ("::" ^ cpp_member_name_of member)))
    | VarThis (member, _) -> out ("this->" ^ cpp_member_name_of member)
    | VarInstance (obj, member, _, operator) ->
        gen obj;
        out (operator ^ cpp_member_name_of member)
    | VarInternal (obj, operator, member) ->
        gen obj;
        out (operator ^ member)
    | VarInterface (obj, member) ->
        gen obj;
        out ("->" ^ cpp_member_name_of member ^ "_get()")
  and string_of_op_eq op pos =
    match op with
    | OpAdd -> "::hx::AddEq"
    | OpMult -> "::hx::MultEq"
    | OpDiv -> "::hx::DivEq"
    | OpSub -> "::hx::SubEq"
    | OpAnd -> "::hx::AndEq"
    | OpOr -> "::hx::OrEq"
    | OpXor -> "::hx::XorEq"
    | OpShl -> "::hx::ShlEq"
    | OpShr -> "::hx::ShrEq"
    | OpUShr -> "::hx::UShrEq"
    | OpMod -> "::hx::ModEq"
    | _ -> abort "Bad assign op" pos
  and string_of_op op pos =
    match op with
    | OpAdd -> "+"
    | OpMult -> "*"
    | OpDiv -> "/"
    | OpSub -> "-"
    | OpEq -> "=="
    | OpNotEq -> "!="
    | OpGt -> ">"
    | OpGte -> ">="
    | OpLt -> "<"
    | OpLte -> "<="
    | OpAnd -> "&"
    | OpOr -> "|"
    | OpXor -> "^"
    | OpBoolAnd -> "&&"
    | OpBoolOr -> "||"
    | OpShl -> "<<"
    | OpShr -> ">>"
    | OpUShr -> ">>>"
    | OpMod -> "%"
    | OpInterval -> "..."
    | OpArrow -> "->"
    | OpIn -> " in "
    | OpNullCoal -> "??"
    | OpAssign | OpAssignOp _ -> abort "Unprocessed OpAssign" pos
  and gen_closure closure =
    let argc = Hashtbl.length closure.close_undeclared in
    let size = string_of_int argc in
    if argc >= 62 then
      (* Limited by c++ macro size of 128 args *)
      abort "Too many capture variables" closure.close_expr.cpppos;
    if argc >= 20 || List.length closure.close_args >= 20 then
      writer#add_big_closures;
    let argsCount = list_num closure.close_args in
    output_i ("HX_BEGIN_LOCAL_FUNC_S" ^ size ^ "(");
    out
      (if closure.close_this != None then "::hx::LocalThisFunc,"
       else "::hx::LocalFunc,");
    out ("_hx_Closure_" ^ string_of_int closure.close_id);
    Hashtbl.iter
      (fun name var ->
        out ("," ^ cpp_macro_var_type_of var ^ "," ^ keyword_remap name))
      closure.close_undeclared;
    out (") HXARGC(" ^ argsCount ^ ")\n");

    let func_type = tcpp_to_string closure.close_type in
    output_i
      (func_type ^ " _hx_run(" ^ cpp_arg_list closure.close_args "__o_" ^ ")");

    let prologue = function
      | gc_stack ->
          cpp_gen_default_values ctx closure.close_args "__o_";
          hx_stack_push ctx output_i class_name func_name
            closure.close_expr.cpppos gc_stack;
          if ctx.ctx_debug_level >= 2 then (
            if closure.close_this != None then
              output_i "HX_STACK_THIS(__this.mPtr)\n";
            List.iter
              (fun (v, _) ->
                output_i
                  ("HX_STACK_ARG(" ^ cpp_var_name_of v ^ ",\""
                 ^ cpp_debug_name_of v ^ "\")\n"))
              (List.filter (cpp_debug_var_visible ctx) closure.close_args);

            let line = Lexer.get_error_line closure.close_expr.cpppos in
            let lineName = Printf.sprintf "%4d" line in
            out ("HXLINE(" ^ lineName ^ ")\n"))
    in
    gen_with_injection (mk_injection prologue "" "") closure.close_expr true;

    let return =
      match closure.close_type with TCppVoid -> "(void)" | _ -> "return"
    in

    output_i ("HX_END_LOCAL_FUNC" ^ argsCount ^ "(" ^ return ^ ")\n\n")
  in

  gen_with_injection injection cppTree true

let gen_cpp_init ctx dot_name func_name var_name expr =
  let output = ctx.ctx_output in
  let prologue = function
    | gc_stack ->
        let spacer =
          if ctx.ctx_debug_level > 0 then "            \t" else "\t"
        in
        let output_i s = output (spacer ^ s) in
        hx_stack_push ctx output_i dot_name func_name expr.epos gc_stack
  in
  let injection = mk_injection prologue var_name "" in
  gen_cpp_ast_expression_tree ctx dot_name func_name [] t_dynamic injection
    (mk_block expr)

let generate_main_header output_main =
  output_main "#include <hxcpp.h>\n\n";
  output_main "#include <stdio.h>\n\n";
  output_main "extern \"C\" void __hxcpp_main();\n\n";
  output_main "extern \"C\" void __hxcpp_lib_main();\n\n"

let generate_main_footer1 output_main = output_main "void __hxcpp_main() {\n"

let generate_main_footer2 output_main =
  output_main "\t}\n\n";
  output_main "void __hxcpp_lib_main() {\n";
  output_main "\tHX_TOP_OF_STACK\n";
  output_main "\t::hx::Boot();\n";
  output_main "\t__boot_all();\n";
  output_main "\t__hxcpp_main();\n";
  output_main "\t}\n"

let generate_main ctx super_deps class_def =
  let common_ctx = ctx.ctx_common in
  (* main routine should be a single static function *)
  let main_expression =
    match class_def.cl_ordered_statics with
    | [ { cf_expr = Some expression } ] -> expression
    | _ -> die "" __LOC__
  in
  CppReferences.find_referenced_types ctx (TClassDecl class_def) super_deps
    (Hashtbl.create 0) false false false
  |> ignore;
  let depend_referenced =
    CppReferences.find_referenced_types ctx (TClassDecl class_def) super_deps
      (Hashtbl.create 0) false true false
  in
  let generate_startup filename is_main =
    (*make_class_directories base_dir ( "src" :: []);*)
    let cpp_file = new_cpp_file common_ctx common_ctx.file ([], filename) in
    let output_main = cpp_file#write in

    generate_main_header cpp_file#write_h;

    List.iter (add_include cpp_file) depend_referenced;
    output_main "\n\n";

    if is_main then output_main "\n#include <hx/HxcppMain.h>\n\n";

    generate_main_footer1 output_main;

    let ctx = file_context ctx cpp_file 1 false in
    gen_cpp_init ctx "hxcpp" "__hxcpp_main" "" main_expression;

    generate_main_footer2 output_main;
    cpp_file#close
  in
  generate_startup "__main__" true;
  generate_startup "__lib__" false

let generate_dummy_main common_ctx =
  let generate_startup filename is_main =
    let main_file = new_cpp_file common_ctx common_ctx.file ([], filename) in
    let output_main = main_file#write in
    generate_main_header main_file#write_h;
    if is_main then output_main "\n#include <hx/HxcppMain.h>\n\n";
    generate_main_footer1 output_main;
    generate_main_footer2 output_main;
    main_file#close
  in
  generate_startup "__main__" true;
  generate_startup "__lib__" false

let generate_boot ctx boot_enums boot_classes nonboot_classes init_classes =
  let common_ctx = ctx.ctx_common in
  (* Write boot class too ... *)
  let base_dir = common_ctx.file in
  let boot_file = new_cpp_file common_ctx base_dir ([], "__boot__") in
  let output_boot = boot_file#write in
  boot_file#write_h "#include <hxcpp.h>\n\n";

  List.iter
    (fun class_path -> boot_file#add_include class_path)
    (boot_enums @ boot_classes @ nonboot_classes);

  let newScriptable = Common.defined common_ctx Define.Scriptable in
  if newScriptable then (
    output_boot "#include <hx/Scriptable.h>\n";
    let funcs =
      hash_iterate !(ctx.ctx_interface_slot) (fun name id -> (name, id))
    in
    let sorted = List.sort (fun (_, id1) (_, id2) -> id1 - id2) funcs in
    output_boot
      "static const char *scriptableInterfaceFuncs[] = {\n\t0,\n\t0,\n";
    List.iter
      (fun (name, id) ->
        output_boot ("\t\"" ^ name ^ "\", //" ^ string_of_int (-id) ^ "\n"))
      sorted;
    output_boot "};\n");

  output_boot "\nvoid __files__boot();\n";
  output_boot "\nvoid __boot_all()\n{\n";
  output_boot "__files__boot();\n";
  output_boot "::hx::RegisterResources( ::hx::GetResources() );\n";
  if newScriptable then
    output_boot
      ("::hx::ScriptableRegisterNameSlots(scriptableInterfaceFuncs,"
      ^ string_of_int !(ctx.ctx_interface_slot_count)
      ^ ");\n");

  List.iter
    (fun class_path ->
      output_boot
        ("::" ^ join_class_path_remap class_path "::" ^ "_obj::__register();\n"))
    (boot_enums @ boot_classes @ nonboot_classes);

  let dump_boot =
    List.iter (fun class_path ->
        output_boot
          ("::" ^ join_class_path_remap class_path "::" ^ "_obj::__boot();\n"))
  in

  dump_boot boot_enums;

  List.iter
    (fun class_path ->
      output_boot
        ("::" ^ join_class_path_remap class_path "::" ^ "_obj::__init__();\n"))
    (List.rev init_classes);

  let is_cpp_class = function
    | "cpp" :: _, _ -> true
    | [], "EReg" -> true
    | [ "haxe" ], "Log" -> true
    | _ -> false
  in

  dump_boot
    (List.filter (fun path -> is_cpp_class path) (List.rev boot_classes));
  dump_boot
    (List.filter (fun path -> not (is_cpp_class path)) (List.rev boot_classes));

  output_boot "}\n\n";
  boot_file#close

let generate_files common_ctx file_info =
  (* Write __files__ class too ... *)
  let base_dir = common_ctx.file in
  let files_file = new_cpp_file common_ctx base_dir ([], "__files__") in
  let output_files = files_file#write in
  let types = common_ctx.types in
  files_file#write_h "#include <hxcpp.h>\n\n";
  output_files "namespace hx {\n";
  output_files "const char *__hxcpp_all_files[] = {\n";
  output_files "#ifdef HXCPP_DEBUGGER\n";
  List.iter
    (fun file -> output_files (const_char_star file ^ ",\n"))
    (List.sort String.compare (pmap_keys !file_info));
  output_files "#endif\n";
  output_files " 0 };\n";
  output_files "\n";

  output_files "const char *__hxcpp_all_files_fullpath[] = {\n";
  output_files "#ifdef HXCPP_DEBUGGER\n";
  List.iter
    (fun file ->
      output_files
        (const_char_star
           (Path.get_full_path
              (try Common.find_file common_ctx file with Not_found -> file))
        ^ ",\n"))
    (List.sort String.compare (pmap_keys !file_info));
  output_files "#endif\n";
  output_files " 0 };\n";
  output_files "\n";

  output_files "const char *__hxcpp_all_classes[] = {\n";
  output_files "#ifdef HXCPP_DEBUGGER\n";
  List.iter
    (fun object_def ->
      match object_def with
      | TClassDecl class_def when is_extern_class class_def -> ()
      | TClassDecl class_def when has_class_flag class_def CInterface -> ()
      | TClassDecl class_def ->
          output_files
            (const_char_star (join_class_path class_def.cl_path ".") ^ ",\n")
      | _ -> ())
    types;
  output_files "#endif\n";
  output_files " 0 };\n";

  output_files "} // namespace hx\n";
  output_files
    "void __files__boot() { \
     __hxcpp_set_debugger_info(::hx::__hxcpp_all_classes, \
     ::hx::__hxcpp_all_files_fullpath); }\n";

  files_file#close

let gen_cpp_function_body ctx clazz is_static func_name function_def head_code
    tail_code no_debug =
  let output = ctx.ctx_output in
  let dot_name = join_class_path clazz.cl_path "." in
  if no_debug then ctx.ctx_debug_level <- 0;
  let prologue = function
    | gc_stack ->
        let spacer = if no_debug then "\t" else "            \t" in
        let output_i s = output (spacer ^ s) in
        ctx_default_values ctx function_def.tf_args "__o_";
        hx_stack_push ctx output_i dot_name func_name function_def.tf_expr.epos
          gc_stack;
        if ctx.ctx_debug_level >= 2 then (
          if not is_static then
            output_i
              ("HX_STACK_THIS("
              ^ (if ctx.ctx_real_this_ptr then "this" else "__this")
              ^ ")\n");
          List.iter
            (fun (v, _) ->
              if not (cpp_no_debug_synbol ctx v) then
                output_i
                  ("HX_STACK_ARG(" ^ cpp_var_name_of v ^ ",\"" ^ v.v_name
                 ^ "\")\n"))
            function_def.tf_args;

          let line = Lexer.get_error_line function_def.tf_expr.epos in
          let lineName = Printf.sprintf "%4d" line in
          output ("HXLINE(" ^ lineName ^ ")\n"));
        if head_code <> "" then output_i (head_code ^ "\n")
  in
  let args = List.map fst function_def.tf_args in

  let injection = mk_injection prologue "" tail_code in
  gen_cpp_ast_expression_tree ctx dot_name func_name args function_def.tf_type
    injection
    (mk_block function_def.tf_expr)

let constructor_arg_var_list class_def =
  match class_def.cl_constructor with
  | Some definition -> (
      match definition.cf_expr with
      | Some { eexpr = TFunction function_def } ->
          List.map
            (fun (v, o) ->
              (v.v_name, type_arg_to_string v.v_name o v.v_type "__o_"))
            function_def.tf_args
      | _ -> (
          match follow definition.cf_type with
          | TFun (args, _) ->
              List.map (fun (a, _, t) -> (a, (type_to_string t, a))) args
          | _ -> []))
  | _ -> []

let generate_constructor ctx out class_def isHeader =
  let class_name = class_name class_def in
  let ptr_name = class_pointer class_def in
  let can_quick_alloc = can_quick_alloc class_def in
  let gcName = gen_gc_name class_def.cl_path in
  let isContainer = if has_gc_references class_def then "true" else "false" in
  let cargs = constructor_arg_var_list class_def in
  let constructor_type_var_list = List.map snd cargs in
  let constructor_type_args =
    String.concat ","
      (List.map (fun (t, a) -> t ^ " " ^ a) constructor_type_var_list)
  in
  let constructor_var_list = List.map snd constructor_type_var_list in
  let constructor_args = String.concat "," constructor_var_list in

  let classScope = if isHeader then "" else class_name ^ "::" in
  let staticHead = if isHeader then "inline static " else "" in
  out
    (staticHead ^ ptr_name ^ " " ^ classScope ^ "__new(" ^ constructor_type_args
   ^ ") {\n");
  out ("\t" ^ ptr_name ^ " __this = new " ^ class_name ^ "();\n");
  out ("\t__this->__construct(" ^ constructor_args ^ ");\n");
  out "\treturn __this;\n";
  out "}\n\n";

  if can_quick_alloc then (
    out
      (staticHead ^ ptr_name ^ " " ^ classScope ^ "__alloc(::hx::Ctx *_hx_ctx"
      ^ (if constructor_type_args = "" then "" else "," ^ constructor_type_args)
      ^ ") {\n");
    out
      ("\t" ^ class_name ^ " *__this = (" ^ class_name
     ^ "*)(::hx::Ctx::alloc(_hx_ctx, sizeof(" ^ class_name ^ "), " ^ isContainer
     ^ ", " ^ gcName ^ "));\n");
    out ("\t*(void **)__this = " ^ class_name ^ "::_hx_vtable;\n");
    let rec dump_dynamic class_def =
      if has_dynamic_member_functions class_def then
        out
          ("\t"
          ^ join_class_path_remap class_def.cl_path "::"
          ^ "_obj::__alloc_dynamic_functions(_hx_ctx,__this);\n")
      else
        match class_def.cl_super with
        | Some super -> dump_dynamic (fst super)
        | _ -> ()
    in
    dump_dynamic class_def;

    if isHeader then
      match class_def.cl_constructor with
      | Some
          ({ cf_expr = Some { eexpr = TFunction function_def } } as definition)
        ->
          with_debug ctx definition.cf_meta (fun no_debug ->
              ctx.ctx_real_this_ptr <- false;
              gen_cpp_function_body ctx class_def false "new" function_def "" ""
                no_debug;
              out "\n")
      | _ -> ()
    else out ("\t__this->__construct(" ^ constructor_args ^ ");\n");

    out "\treturn __this;\n";
    out "}\n\n")

let generate_native_constructor ctx out class_def isHeader =
  let cargs = constructor_arg_var_list class_def in
  let constructor_type_var_list = List.map snd cargs in
  let constructor_type_args =
    String.concat ","
      (List.map (fun (t, a) -> t ^ " " ^ a) constructor_type_var_list)
  in
  let class_name = class_name class_def in

  match class_def.cl_constructor with
  | Some ({ cf_expr = Some { eexpr = TFunction function_def } } as definition)
    ->
      if isHeader then
        out ("\t\t" ^ class_name ^ "(" ^ constructor_type_args ^ ");\n\n")
      else
        with_debug ctx definition.cf_meta (fun no_debug ->
            ctx.ctx_real_this_ptr <- true;
            out
              (class_name ^ "::" ^ class_name ^ "(" ^ constructor_type_args
             ^ ")");

            (match class_def.cl_super with
            | Some (klass, _) -> (
                let rec find_super_args = function
                  | TCall ({ eexpr = TConst TSuper }, args) :: _ -> Some args
                  | (TParenthesis e | TMeta (_, e) | TCast (e, None)) :: rest ->
                      find_super_args (e.eexpr :: rest)
                  | TBlock e :: rest ->
                      find_super_args (List.map (fun e -> e.eexpr) e @ rest)
                  | _ :: rest -> find_super_args rest
                  | _ -> None
                in
                match find_super_args [ function_def.tf_expr.eexpr ] with
                | Some args ->
                    out ("\n:" ^ cpp_class_path_of klass [] ^ "(");
                    let sep = ref "" in
                    List.iter
                      (fun arg ->
                        out !sep;
                        sep := ",";
                        gen_cpp_ast_expression_tree ctx "" "" [] t_dynamic None
                          arg)
                      args;
                    out ")\n"
                | _ -> ())
            | _ -> ());

            let head_code = get_code definition.cf_meta Meta.FunctionCode in
            let tail_code = get_code definition.cf_meta Meta.FunctionTailCode in
            gen_cpp_function_body ctx class_def false "new" function_def
              head_code tail_code no_debug)
  | _ -> ()

let dynamic_functions class_def =
  List.fold_left
    (fun result field ->
      match field.cf_expr with
      | Some { eexpr = TFunction function_def }
        when is_dynamic_haxe_method field ->
          keyword_remap field.cf_name :: result
      | _ -> result)
    [] class_def.cl_ordered_fields