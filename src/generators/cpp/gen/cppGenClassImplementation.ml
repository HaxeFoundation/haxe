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
open CppGen

let gen_function ctx class_def class_name is_static (field, function_def) =
  let output          = ctx.ctx_output in
  let nargs           = string_of_int (List.length function_def.tf_args) in
  let return_type_str = type_to_string function_def.tf_type in
  let return_type     = cpp_type_of function_def.tf_type in
  let no_debug        = Meta.has Meta.NoDebug field.cf_meta in
  let is_void         = return_type = TCppVoid in
  let ret             = if is_void then "(void)" else "return " in
  let needsWrapper t =
    match t with
    | TCppStar _ -> true
    | TCppInst (t, _) -> Meta.has Meta.StructAccess t.cl_meta
    | _ -> false
  in

  (* The actual function definition *)
  let remap_name = native_field_name_remap is_static field in
  output (if is_void then "void" else return_type_str);
  output (" " ^ class_name ^ "::" ^ remap_name ^ "(");
  output (print_arg_list function_def.tf_args "__o_");
  output ")";
  ctx.ctx_real_this_ptr <- true;
  let code = get_code field.cf_meta Meta.FunctionCode in
  let tail_code = get_code field.cf_meta Meta.FunctionTailCode in

  match get_meta_string field.cf_meta Meta.Native with
  | Some nativeImpl when is_static ->
    output " {\n";
    output
      ("\t" ^ ret ^ "::" ^ nativeImpl ^ "("
      ^ print_arg_list_name function_def.tf_args "__o_"
      ^ ");\n");
    output "}\n\n"
  | _ ->
    gen_cpp_function_body ctx class_def is_static field.cf_name
      function_def code tail_code no_debug;

    output "\n\n";
    let nonVirtual = Meta.has Meta.NonVirtual field.cf_meta in
    let doDynamic =
      (nonVirtual || not (is_override field))
      && reflective class_def field
    in
    (* generate dynamic version too ... *)
    if doDynamic then
      let tcpp_args =
        List.map
          (fun (v, _) -> cpp_type_of v.v_type)
          function_def.tf_args
      in
      let wrap = needsWrapper return_type || List.exists needsWrapper tcpp_args in
      if wrap then (
        let wrapName = "_hx_wrap" ^ class_name ^ "_" ^ remap_name in
        output ("static ::Dynamic " ^ wrapName ^ "( ");

        let initial = if is_static then [] else [ "::hx::Object *obj" ] in

        initial
        |> List.append (List.init (List.length tcpp_args) (fun idx -> Printf.sprintf "const ::Dynamic &a%i" idx))
        |> String.concat ","
        |> output;

        output ") {\n\t";
        (if not is_void then
            match return_type with
            | TCppStar _ -> output "return (cpp::Pointer<const void *>) "
            | TCppInst (t, _) when Meta.has Meta.StructAccess t.cl_meta
              ->
                output
                  ("return (cpp::Struct< " ^ tcpp_to_string return_type
                ^ " >) ")
            | _ -> output "return ");

        if is_static then
          output (class_name ^ "::" ^ remap_name ^ "(")
        else
          output
            ("reinterpret_cast< " ^ class_name ^ " *>(obj)->"
            ^ remap_name ^ "(");

        let cast_prefix arg =
          match arg with
          | TCppStar (t, const) ->
              Printf.sprintf "(::cpp::%sPointer< %s >)" (if const then "Const" else "") (tcpp_to_string arg)
          | TCppInst (t, _) when Meta.has Meta.StructAccess t.cl_meta ->
            Printf.sprintf "(::cpp::Struct< %s >)" (tcpp_to_string arg)
          | _ ->
            "" in
        tcpp_args
        |> List.map cast_prefix
        |> List.map2
          (fun prefix arg -> prefix ^ arg)
          (List.init (List.length tcpp_args) (fun idx -> Printf.sprintf "a%i" idx))
        |> String.concat ", "
        |> output;

        output ");\n";

        if is_void then output "\treturn null();\n";
        output "}\n";
        let nName = string_of_int (List.length tcpp_args) in
        output
          ("::Dynamic " ^ class_name ^ "::" ^ remap_name
          ^ "_dyn() {\n\treturn ");
        if is_static then
          output
            ("::hx::CreateStaticFunction" ^ nName ^ "(\"" ^ remap_name
            ^ "\"," ^ wrapName ^ ");")
        else
          output
            ("::hx::CreateMemberFunction" ^ nName ^ "(\"" ^ remap_name
            ^ "\",this," ^ wrapName ^ ");");
        output "}\n")
      else
        let prefix = if is_static then "STATIC_" else "" in
        Printf.sprintf "%sHX_DEFINE_DYNAMIC_FUNC%s(%s, %s, %s)\n\n" prefix nargs class_name remap_name ret |> output

let gen_dynamic_function ctx class_def class_name is_static is_for_static_var (field, function_def) =
  let output = ctx.ctx_output in
  let remap_name = keyword_remap field.cf_name in
  let func_name = "__default_" ^ remap_name in
  let nargs = string_of_int (List.length function_def.tf_args) in
  let return_type_str = type_to_string function_def.tf_type in
  let return_type = cpp_type_of function_def.tf_type in
  let no_debug = Meta.has Meta.NoDebug field.cf_meta in
  let is_void = return_type = TCppVoid in
  let ret = if is_void then "(void)" else "return " in

  ctx.ctx_real_this_ptr <- false;
  Printf.sprintf "HX_BEGIN_DEFAULT_FUNC(%s, %s)\n" func_name class_name |> output; 
  Printf.sprintf "%s _hx_run(%s)" return_type_str (print_arg_list function_def.tf_args "__o_") |> output;

  gen_cpp_function_body ctx class_def is_static func_name function_def "" "" no_debug;

  output ("HX_END_LOCAL_FUNC" ^ nargs ^ "(" ^ ret ^ ")\n");
  output "HX_END_DEFAULT_FUNC\n\n"

let gen_static_variable ctx class_def class_name field =
  let output = ctx.ctx_output in
  let remap_name = keyword_remap field.cf_name in
  gen_type ctx field.cf_type;
  output (" " ^ class_name ^ "::" ^ remap_name ^ ";\n\n")

let gen_abstract_function ctx class_def class_name (field, tl, tr) =
  let output = ctx.ctx_output in
  let remap_name = keyword_remap field.cf_name in
  let return_type = cpp_type_of tr in
  let is_void = return_type = TCppVoid in
  let ret = if is_void then "(void)" else "return " in
  Printf.sprintf "HX_DEFINE_DYNAMIC_FUNC%i(%s, %s, %s)\n\n" (List.length tl) class_name remap_name ret |> output

let gen_field_init ctx class_def field =
  let dot_name   = join_class_path class_def.cl_path "." in
  let output     = ctx.ctx_output in
  let remap_name = keyword_remap field.cf_name in

  match field.cf_expr with
  (* Function field *)
  | Some { eexpr = TFunction function_def } ->
    if is_dynamic_haxe_method field then
      let func_name = "__default_" ^ remap_name in
      output ("\t" ^ remap_name ^ " = new " ^ func_name ^ ";\n\n")
  (* Data field *)
  | Some expr ->
      gen_cpp_init ctx dot_name "boot" (remap_name ^ " = ") expr
  | _ -> ()

let gen_boot_field ctx output_cpp tcpp_class =
  if has_boot_field tcpp_class.tcl_class then (
    output_cpp ("void " ^ tcpp_class.tcl_name ^ "::__boot()\n{\n");

    let dot_name = join_class_path tcpp_class.tcl_class.cl_path "." in

    (match tcpp_class.tcl_meta with
    | Some expr -> gen_cpp_init ctx dot_name "boot" "__mClass->__meta__ = " expr
    | None -> ());

    (match tcpp_class.tcl_rtti with
    | Some expr -> gen_cpp_init ctx dot_name "boot" "__mClass->__rtti__ = " expr
    | None -> ());

    List.iter (gen_field_init ctx tcpp_class.tcl_class) tcpp_class.tcl_static_variables;

    tcpp_class.tcl_static_dynamic_functions
    |> List.map fst
    |> List.iter (gen_field_init ctx tcpp_class.tcl_class);

    output_cpp "}\n\n")

let gen_init_function ctx output_cpp tcpp_class =
  match tcpp_class.tcl_init with
  | Some expression ->
    output_cpp ("void " ^ tcpp_class.tcl_name ^ "::__init__()");
    gen_cpp_init ctx (cpp_class_name tcpp_class.tcl_class) "__init__" "" (mk_block expression);
    output_cpp "\n\n"
  | None ->
    ()

let gen_dynamic_function_allocator ctx output_cpp tcpp_class =
  match tcpp_class.tcl_dynamic_functions with
  | [] -> ()
  | functions ->
    let mapper (field, _) =
      let name = keyword_remap field.cf_name in
      Printf.sprintf "\tif (!_hx_obj->%s.mPtr) { _hx_obj->%s = new __default_%s(_hx_obj); }" name name name in   
    let rec folder acc class_def =
      if has_dynamic_member_functions class_def then
        let super_name = join_class_path_remap class_def.cl_path "::" ^ "_obj" in

        Printf.sprintf "\t%s::__alloc_dynamic_functions(_hx_ctx, _hx_obj);" super_name :: acc
      else
        match class_def.cl_super with
        | Some (super, _) -> folder acc super
        | _ -> acc
      in

    let initial = functions |> List.map mapper in
    let allocs  = match tcpp_class.tcl_class.cl_super with
    | Some (super, _) ->
      folder initial super
    | _ ->
      initial in

    let str = allocs |> List.rev |> String.concat "\n" in

    Printf.sprintf "void %s::__alloc_dynamic_functions(::hx::Ctx* _hx_ctx, %s* _hx_obj) {\n%s\n}\n" tcpp_class.tcl_name tcpp_class.tcl_name str |> output_cpp

let print_reflective_fields ctx_common class_def variables functions abstract_functions =
  let strq = strq ctx_common in

  let filter_vars field =
    if reflective class_def field then
      Some (Printf.sprintf "\t%s" (strq field.cf_name))
    else
      None in
  let filter_funcs (field, _) =
    if reflective class_def field then
      Some (Printf.sprintf "\t%s" (strq field.cf_name))
    else
      None in
  let filter_abst (field, _, _) =
    if reflective class_def field then
      Some (Printf.sprintf "\t%s" (strq field.cf_name))
    else
      None in

  let reflective_variables = variables |> List.filter_map filter_vars in
  let reflective_functions = functions |> List.filter_map filter_funcs in
  let reflective_abstracts = abstract_functions |> List.filter_map filter_abst in

  match reflective_variables @ reflective_functions @ reflective_abstracts with
  | [] ->
    None
  | concat ->
    Some (concat @ [ "\t::String(null())" ] |> String.concat ",\n")

let generate_native_class base_ctx tcpp_class =
  let class_def = tcpp_class.tcl_class in
  let class_path = class_def.cl_path in
  let debug = tcpp_class.tcl_debug_level in
  let cpp_file = new_placed_cpp_file base_ctx.ctx_common class_path in
  let cpp_ctx = file_context base_ctx cpp_file debug false in
  let ctx = cpp_ctx in
  let output_cpp = cpp_file#write in
  let scriptable = has_tcpp_class_flag tcpp_class Scriptable in

  if debug > 1 then
    print_endline
      ("Found class definition:" ^ join_class_path class_def.cl_path "::");

  cpp_file#write_h "#include <hxcpp.h>\n\n";

  let all_referenced =
    CppReferences.find_referenced_types ctx (TClassDecl class_def) ctx.ctx_super_deps
    ctx.ctx_constructor_deps false false scriptable
  in
  List.iter (add_include cpp_file) all_referenced;

  if scriptable then cpp_file#write_h "#include <hx/Scriptable.h>\n";

  cpp_file#write_h "\n";

  output_cpp (get_class_code class_def Meta.CppFileCode);
  let includes = get_all_meta_string_path class_def.cl_meta Meta.CppInclude in
  let printer inc = output_cpp ("#include \"" ^ inc ^ "\"\n") in
  List.iter printer includes;

  begin_namespace output_cpp class_path;
  output_cpp "\n";

  output_cpp (get_class_code class_def Meta.CppNamespaceCode);

  let class_name = tcpp_class.tcl_name in

  gen_init_function ctx output_cpp tcpp_class;

  List.iter (gen_function ctx class_def class_name false) tcpp_class.tcl_functions;
  List.iter (gen_dynamic_function ctx class_def class_name false false) tcpp_class.tcl_dynamic_functions;
  List.iter (gen_abstract_function ctx class_def class_name) tcpp_class.tcl_abstract_functions;

  List.iter (gen_function ctx class_def class_name true) tcpp_class.tcl_static_functions;
  List.iter (gen_dynamic_function ctx class_def class_name true false) tcpp_class.tcl_static_dynamic_functions;
  List.iter (gen_static_variable ctx class_def class_name) tcpp_class.tcl_static_variables;

  output_cpp "\n";

  gen_dynamic_function_allocator ctx output_cpp tcpp_class;
  
  generate_native_constructor ctx output_cpp class_def false;
  gen_boot_field ctx output_cpp tcpp_class;

  end_namespace output_cpp class_path;

  cpp_file#close

let generate_managed_class base_ctx tcpp_class =
  let common_ctx = base_ctx.ctx_common in
  let class_def = tcpp_class.tcl_class in
  let class_path = class_def.cl_path in
  let debug = tcpp_class.tcl_debug_level in
  let cpp_file = new_placed_cpp_file base_ctx.ctx_common class_path in
  let cpp_ctx = file_context base_ctx cpp_file debug false in
  let ctx = cpp_ctx in
  let output_cpp = cpp_file#write in
  let strq = strq ctx.ctx_common in
  let scriptable = has_tcpp_class_flag tcpp_class Scriptable in

  let class_super_name =
    match class_def.cl_super with
    | Some (klass, params) ->
        tcpp_to_string_suffix "_obj" (cpp_instance_type klass params)
    | _ -> ""
  in
  if debug > 1 then
    print_endline
      ("Found class definition:" ^ join_class_path class_def.cl_path "::");

  cpp_file#write_h "#include <hxcpp.h>\n\n";

  let all_referenced =
    CppReferences.find_referenced_types ctx (TClassDecl class_def) ctx.ctx_super_deps
    ctx.ctx_constructor_deps false false scriptable
  in
  List.iter (add_include cpp_file) all_referenced;

  if scriptable then cpp_file#write_h "#include <hx/Scriptable.h>\n";

  cpp_file#write_h "\n";

  output_cpp (get_class_code class_def Meta.CppFileCode);
  let includes = get_all_meta_string_path class_def.cl_meta Meta.CppInclude in
  let printer inc = output_cpp ("#include \"" ^ inc ^ "\"\n") in
  List.iter printer includes;

  begin_namespace output_cpp class_path;
  output_cpp "\n";

  output_cpp (get_class_code class_def Meta.CppNamespaceCode);

  let class_name = tcpp_class.tcl_name in
  let cargs = constructor_arg_var_list class_def in
  let constructor_var_list = List.map snd cargs in
  let constructor_type_args =
    cargs
      |> List.map (fun (t, a) -> Printf.sprintf "%s %s" t a)
      |> String.concat "," in

  output_cpp
    ("void " ^ class_name ^ "::__construct(" ^ constructor_type_args ^ ")");
  (match class_def.cl_constructor with
  | Some ({ cf_expr = Some { eexpr = TFunction function_def } } as definition)
    ->
      with_debug ctx definition.cf_meta (fun no_debug ->
          gen_cpp_function_body ctx class_def false "new" function_def "" ""
            no_debug;
          output_cpp "\n")
  | _ -> output_cpp " { }\n\n");

  (* Destructor goes in the cpp file so we can "see" the full definition of the member vars *)
  if not (has_class_flag class_def CAbstract) then (
    let ptr_name = class_pointer class_def in
    let array_arg_list inList =
      List.init (List.length inList) (fun idx -> Printf.sprintf "inArgs[%i]" idx) |> String.concat ","
    in

    Printf.sprintf "::Dynamic %s::__CreateEmpty() { return new %s; }\n\n" class_name class_name |> output_cpp;

    Printf.sprintf "void* %s::_hx_vtable = 0;\n\n" class_name |> output_cpp;

    Printf.sprintf "::Dynamic %s::__Create(::hx::DynamicArray inArgs)\n" class_name |> output_cpp;
    Printf.sprintf "{\n\t%s _hx_result = new %s();\n" ptr_name class_name |> output_cpp;
    Printf.sprintf "\t_hx_result->__construct(%s);\n" (array_arg_list constructor_var_list) |> output_cpp;
    output_cpp "\treturn _hx_result;\n}\n\n");

  output_cpp ("bool " ^ class_name ^ "::_hx_isInstanceOf(int inClassId) {\n");
  let implemented_classes = List.sort compare ((Int32.of_int 1) :: tcpp_class.tcl_id :: tcpp_class.tcl_parent_ids) in
  let txt cId = Printf.sprintf "0x%08lx" cId in
  let rec dump_classes indent classes =
    match classes with
    | [] -> ()
    | [ c ] -> output_cpp (indent ^ "return inClassId==(int)" ^ txt c ^ ";\n")
    | [ c; c1 ] ->
        output_cpp
          (indent ^ "return inClassId==(int)" ^ txt c ^ " || inClassId==(int)" ^ txt c1 ^ ";\n")
    | _ ->
        let len = List.length classes in
        let mid = List.nth classes (len / 2) in
        let low, high = List.partition (fun e -> e <= mid) classes in
        output_cpp (indent ^ "if (inClassId<=(int)" ^ txt mid ^ ") {\n");
        dump_classes (indent ^ "\t") low;
        output_cpp (indent ^ "} else {\n");
        dump_classes (indent ^ "\t") high;
        output_cpp (indent ^ "}\n")
  in
  dump_classes "\t" implemented_classes;
  output_cpp "}\n\n";

  if List.length tcpp_class.tcl_haxe_parents > 0 then (
    let alreadyGlued = Hashtbl.create 0 in
    let cname = "_hx_" ^ join_class_path class_def.cl_path "_" in
    let implname = cpp_class_name class_def in
    let cpp_glue = ref [] in
    let iter interface =
      let interface_name = cpp_interface_impl_name interface in
      output_cpp
        ("static " ^ cpp_class_name interface ^ " " ^ cname ^ "_"
        ^ interface_name ^ "= {\n");
      let rec gen_interface_funcs interface =
        let gen_field field =
          match (follow field.cf_type, field.cf_kind) with
          | _, Method MethDynamic -> ()
          | TFun (args, return_type), Method _ ->
              let cast = cpp_tfun_signature false args return_type in
              let class_implementation =
                find_class_implementation class_def field.cf_name
                  interface
              in
              let realName = cpp_member_name_of field in
              let castKey = realName ^ "::" ^ cast in
              (* C++ can't work out which function it needs to take the addrss of
                  when the implementation is overloaded - currently the map-set functions.
                  Change the castKey to force a glue function in this case (could double-cast the pointer, but it is ugly)
              *)
              let castKey =
                if interface_name = "_hx_haxe_IMap" && realName = "set"
                then castKey ^ "*"
                else castKey
              in
              let implementationKey =
                realName ^ "::" ^ class_implementation
              in
              if castKey <> implementationKey then (
                let glue =
                  Printf.sprintf "%s_%08lx" field.cf_name
                    (gen_hash32 0 cast)
                in
                if not (Hashtbl.mem alreadyGlued castKey) then (
                  Hashtbl.replace alreadyGlued castKey ();
                  let argList = print_tfun_arg_list true args in
                  let returnType = type_to_string return_type in
                  let returnStr =
                    if returnType = "void" then "" else "return "
                  in
                  let cppCode =
                    returnType ^ " " ^ class_name ^ "::" ^ glue ^ "("
                    ^ argList ^ ") {\n" ^ "\t\t\t" ^ returnStr ^ realName
                    ^ "(" ^ print_arg_names args ^ ");\n}\n"
                  in
                  (* let headerCode = "\t\t" ^ returnType ^ " " ^ glue ^ "(" ^ argList ^ ");\n" in *)
                  (* header_glue := headerCode :: !header_glue; *)
                  cpp_glue := cppCode :: !cpp_glue);
                output_cpp
                  ("\t" ^ cast ^ "&" ^ implname ^ "::" ^ glue ^ ",\n"))
              else
                output_cpp
                  ("\t" ^ cast ^ "&" ^ implname ^ "::" ^ realName ^ ",\n")
          | _ -> ()
        in
        (match interface.cl_super with
        | Some super -> gen_interface_funcs (fst super)
        | _ -> ());
        List.iter gen_field interface.cl_ordered_fields
      in
      gen_interface_funcs interface;
      output_cpp "};\n\n" in
    List.iter
      iter
      tcpp_class.tcl_haxe_parents;

    output_cpp (String.concat "\n" !cpp_glue);

    output_cpp ("void *" ^ class_name ^ "::_hx_getInterface(int inHash) {\n");
    output_cpp "\tswitch(inHash) {\n";

    let iter interface =
      output_cpp ("\t\tcase (int)" ^ cpp_class_hash interface ^ ": return &" ^ cname ^ "_" ^ cpp_interface_impl_name interface ^ ";\n") in
    List.iter
      iter
      tcpp_class.tcl_haxe_parents;

    output_cpp "\t}\n";

    if class_super_name = "" then (
      output_cpp "\t#ifdef HXCPP_SCRIPTABLE\n";
      output_cpp "\treturn super::_hx_getInterface(inHash);\n";
      output_cpp "\t#else\n";
      output_cpp "\treturn 0;\n";
      output_cpp "\t#endif\n")
    else output_cpp "\treturn super::_hx_getInterface(inHash);\n";
    output_cpp "}\n\n");

  gen_init_function ctx output_cpp tcpp_class;

  let statics_except_meta = statics_except_meta class_def in

  List.iter (gen_function ctx class_def class_name false) tcpp_class.tcl_functions;
  List.iter (gen_dynamic_function ctx class_def class_name false false) tcpp_class.tcl_dynamic_functions;
  List.iter (gen_abstract_function ctx class_def class_name) tcpp_class.tcl_abstract_functions;

  List.iter (gen_function ctx class_def class_name true) tcpp_class.tcl_static_functions;
  List.iter (gen_dynamic_function ctx class_def class_name true false) tcpp_class.tcl_static_dynamic_functions;
  List.iter (gen_static_variable ctx class_def class_name) tcpp_class.tcl_static_variables;

  output_cpp "\n";

  gen_dynamic_function_allocator ctx output_cpp tcpp_class;

  let inline_constructor =
    can_inline_constructor base_ctx class_def
  in
  if (not inline_constructor) && not (has_class_flag class_def CAbstract) then
    generate_constructor ctx output_cpp tcpp_class false;

  let reflect_member_fields =
    List.filter (reflective class_def) class_def.cl_ordered_fields
  in
  let reflect_static_fields =
    List.filter (reflective class_def) statics_except_meta
  in

  (* Initialise non-static variables *)
  output_cpp (class_name ^ "::" ^ class_name ^ "()\n{\n");
  List.iter
    (fun (field, _) ->
      let name = keyword_remap field.cf_name in
      output_cpp ("\t" ^ name ^ " = new __default_" ^ name ^ "(this);\n"))
    tcpp_class.tcl_dynamic_functions;
  output_cpp "}\n\n";

  if has_tcpp_class_flag tcpp_class Container then (
    let super_needs_iteration = find_next_super_iteration class_def in
    let smart_class_name = snd class_path in
    let dump_field_iterator macro field =
      Printf.sprintf "\t%s(%s, \"%s\");\n" macro (keyword_remap field.cf_name) field.cf_name |> output_cpp
    in
    
    (* MARK function - explicitly mark all child pointers *)
    output_cpp ("void " ^ class_name ^ "::__Mark(HX_MARK_PARAMS)\n{\n");
    output_cpp ("\tHX_MARK_BEGIN_CLASS(" ^ smart_class_name ^ ");\n");
    List.iter (dump_field_iterator "HX_MARK_MEMBER_NAME") tcpp_class.tcl_variables;
    (match super_needs_iteration with
    | None -> ()
    | Some super -> output_cpp ("\t" ^ super ^ "::__Mark(HX_MARK_ARG);\n"));
    output_cpp "\tHX_MARK_END_CLASS();\n";
    output_cpp "}\n\n";

    (* Visit function - explicitly visit all child pointers *)
    output_cpp ("void " ^ class_name ^ "::__Visit(HX_VISIT_PARAMS)\n{\n");
    List.iter (dump_field_iterator "HX_VISIT_MEMBER_NAME") tcpp_class.tcl_variables;
    (match super_needs_iteration with
    | None -> ()
    | Some super -> output_cpp ("\t" ^ super ^ "::__Visit(HX_VISIT_ARG);\n"));
    output_cpp "}\n\n");

  let dump_quick_field_test fields =
    if List.length fields > 0 then (
      let len = function _, l, _ -> l in
      let sfields = List.sort (fun f1 f2 -> len f1 - len f2) fields in
      let len_case = ref (-1) in
      output_cpp "\tswitch(inName.length) {\n";
      List.iter
        (fun (field, l, result) ->
          if l <> !len_case then (
            if !len_case >= 0 then output_cpp "\t\tbreak;\n";
            output_cpp ("\tcase " ^ string_of_int l ^ ":\n");
            len_case := l);
          output_cpp
            ("\t\tif (HX_FIELD_EQ(inName,\""
            ^ StringHelper.s_escape field
            ^ "\") ) { " ^ result ^ " }\n"))
        sfields;
      output_cpp "\t}\n")
  in

  let checkPropCall field =
    if
      Meta.has Meta.NativeProperty class_def.cl_meta
      || Meta.has Meta.NativeProperty field.cf_meta
      || Common.defined common_ctx Define.ForceNativeProperty
    then "inCallProp != ::hx::paccNever"
    else "inCallProp == ::hx::paccAlways"
  in

  let toCommon t f value =
    t ^ "( "
    ^ (match cpp_type_of f.cf_type with
      | TCppInst (t, _) as inst when Meta.has Meta.StructAccess t.cl_meta ->
          "cpp::Struct< " ^ tcpp_to_string inst ^ " >( " ^ value ^ " )"
      | TCppStar (t, _) -> "cpp::Pointer<void *>( " ^ value ^ " )"
      | _ -> value)
    ^ " )"
  in
  let toVal f value = toCommon "::hx::Val" f value in
  let toDynamic f value = toCommon "" f value in

  if has_get_member_field class_def then (
    (* Dynamic "Get" Field function - string version *)
    output_cpp
      ("::hx::Val " ^ class_name
      ^ "::__Field(const ::String &inName,::hx::PropertyAccess inCallProp)\n\
        {\n");
    let get_field_dat =
      List.map (fun f ->
          ( f.cf_name,
            String.length f.cf_name,
            match f.cf_kind with
            | Var { v_read = AccCall } when not (is_physical_field f) ->
                "if (" ^ checkPropCall f ^ ") return "
                ^ toVal f (keyword_remap ("get_" ^ f.cf_name) ^ "()")
                ^ ";"
            | Var { v_read = AccCall } ->
                "return "
                ^ toVal f
                    (checkPropCall f ^ " ? "
                    ^ keyword_remap ("get_" ^ f.cf_name)
                    ^ "() : " ^ keyword_remap f.cf_name
                    ^ if variable_field f then "" else "_dyn()")
                ^ ";"
            | _ ->
                "return "
                ^ toVal f
                    (keyword_remap f.cf_name
                    ^ if variable_field f then "" else "_dyn()")
                ^ ";" ))
    in
    let reflect_member_readable =
      List.filter (is_readable class_def) reflect_member_fields
    in
    dump_quick_field_test (get_field_dat reflect_member_readable);
    output_cpp "\treturn super::__Field(inName,inCallProp);\n}\n\n");

  if has_get_static_field class_def then (
    output_cpp
      ("bool " ^ class_name
      ^ "::__GetStatic(const ::String &inName, Dynamic &outValue, \
        ::hx::PropertyAccess inCallProp)\n\
        {\n");
    let get_field_dat =
      List.map (fun f ->
          ( f.cf_name,
            String.length f.cf_name,
            match f.cf_kind with
            | Var { v_read = AccCall } when not (is_physical_field f) ->
                "if (" ^ checkPropCall f ^ ") { outValue = "
                ^ toDynamic f (keyword_remap ("get_" ^ f.cf_name) ^ "()")
                ^ "; return true; }"
            | Var { v_read = AccCall } ->
                "outValue = "
                ^ toDynamic f
                    (checkPropCall f ^ " ? "
                    ^ keyword_remap ("get_" ^ f.cf_name)
                    ^ "() : " ^ keyword_remap f.cf_name
                    ^ if variable_field f then "" else "_dyn()")
                ^ "; return true;"
            | _ when variable_field f ->
                "outValue = "
                ^ toDynamic f (keyword_remap f.cf_name)
                ^ "; return true;"
            | _ ->
                "outValue = "
                ^ native_field_name_remap true f
                ^ "_dyn(); return true;" ))
    in
    let reflect_static_readable =
      List.filter (is_readable class_def) reflect_static_fields
    in
    dump_quick_field_test (get_field_dat reflect_static_readable);
    output_cpp "\treturn false;\n}\n\n");

  let castable f =
    match cpp_type_of f.cf_type with
    | TCppInst (t, _) as inst when Meta.has Meta.StructAccess t.cl_meta ->
        "cpp::Struct< " ^ tcpp_to_string inst ^ " > "
    | TCppStar (t, _) -> "cpp::Pointer< " ^ tcpp_to_string t ^ " >"
    | _ -> type_to_string f.cf_type
  in

  (* Dynamic "Set" Field function *)
  if has_set_member_field class_def then (
    output_cpp
      ("::hx::Val " ^ class_name
      ^ "::__SetField(const ::String &inName,const ::hx::Val \
        &inValue,::hx::PropertyAccess inCallProp)\n\
        {\n");

    let set_field_dat =
      List.map (fun f ->
          let default_action =
            if is_gc_element ctx (cpp_type_of f.cf_type) then
              "_hx_set_" ^ keyword_remap f.cf_name
              ^ "(HX_CTX_GET,inValue.Cast< " ^ castable f ^ " >());"
              ^ " return inValue;"
            else
              keyword_remap f.cf_name ^ "=inValue.Cast< " ^ castable f
              ^ " >();" ^ " return inValue;"
          in
          ( f.cf_name,
            String.length f.cf_name,
            match f.cf_kind with
            | Var { v_write = AccCall } ->
                let inVal = "(inValue.Cast< " ^ castable f ^ " >())" in
                let setter = keyword_remap ("set_" ^ f.cf_name) in
                "if (" ^ checkPropCall f ^ ") return "
                ^ toVal f (setter ^ inVal)
                ^ ";"
                ^ if not (is_physical_field f) then "" else default_action
            | _ -> default_action ))
    in

    let reflect_member_writable =
      List.filter (is_writable class_def) reflect_member_fields
    in
    let reflect_write_member_variables =
      List.filter variable_field reflect_member_writable
    in
    dump_quick_field_test (set_field_dat reflect_write_member_variables);
    output_cpp "\treturn super::__SetField(inName,inValue,inCallProp);\n}\n\n");

  if has_set_static_field class_def then (
    output_cpp
      ("bool " ^ class_name
      ^ "::__SetStatic(const ::String &inName,Dynamic \
        &ioValue,::hx::PropertyAccess inCallProp)\n\
        {\n");

    let set_field_dat =
      List.map (fun f ->
          let default_action =
            keyword_remap f.cf_name ^ "=ioValue.Cast< " ^ castable f
            ^ " >(); return true;"
          in
          ( f.cf_name,
            String.length f.cf_name,
            match f.cf_kind with
            | Var { v_write = AccCall } ->
                let inVal = "(ioValue.Cast< " ^ castable f ^ " >())" in
                let setter = keyword_remap ("set_" ^ f.cf_name) in
                "if (" ^ checkPropCall f ^ ")  ioValue = "
                ^ toDynamic f (setter ^ inVal)
                ^ ";"
                ^
                if not (is_physical_field f) then ""
                else " else " ^ default_action
            | _ -> default_action ))
    in

    let reflect_static_writable =
      List.filter (is_writable class_def) reflect_static_fields
    in
    let reflect_write_static_variables =
      List.filter variable_field reflect_static_writable
    in
    dump_quick_field_test (set_field_dat reflect_write_static_variables);
    output_cpp "\treturn false;\n}\n\n");

  (* For getting a list of data members (eg, for serialization) *)
  if has_get_fields class_def then (
    let append_field field =
      output_cpp ("\toutFields->push(" ^ strq field.cf_name ^ ");\n")
    in
    let is_data_field field =
      match follow field.cf_type with TFun _ -> false | _ -> true
    in

    output_cpp
      ("void " ^ class_name
      ^ "::__GetFields(Array< ::String> &outFields)\n{\n");
    List.iter append_field
      (List.filter is_data_field class_def.cl_ordered_fields);
    output_cpp "\tsuper::__GetFields(outFields);\n";
    output_cpp "};\n\n");

  let storage field =
    match cpp_type_of field.cf_type with
    | TCppScalar "bool" -> "::hx::fsBool"
    | TCppScalar "int" -> "::hx::fsInt"
    | TCppScalar "Float" -> "::hx::fsFloat"
    | TCppString -> "::hx::fsString"
    | o when is_object_element o ->
        "::hx::fsObject" ^ " /* " ^ tcpp_to_string o ^ " */ "
    | u -> "::hx::fsUnknown" ^ " /* " ^ tcpp_to_string u ^ " */ "
  in
  let dump_member_storage field =
    Printf.sprintf
      "\t{ %s, (int)offsetof(%s, %s), %s },\n" (storage field) class_name (keyword_remap field.cf_name) (strq field.cf_name) |> output_cpp
  in
  let dump_static_storage field =
    Printf.sprintf "\t{ %s, (void*) &%s::%s, %s },\n" (storage field) class_name (keyword_remap field.cf_name) (strq field.cf_name) |> output_cpp
  in

  output_cpp "#ifdef HXCPP_SCRIPTABLE\n";

  if List.length tcpp_class.tcl_variables > 0 then (
    Printf.sprintf "static ::hx::StorageInfo %s_sMemberStorageInfo[] = {\n" class_name |> output_cpp;
    List.iter dump_member_storage tcpp_class.tcl_variables;
    output_cpp "\t{ ::hx::fsUnknown, 0, null()}\n};\n")
  else
    Printf.sprintf "static ::hx::StorageInfo* %s_sMemberStorageInfo = 0;\n" class_name |> output_cpp;

  if List.length tcpp_class.tcl_static_variables > 0 then (
    Printf.sprintf "static ::hx::StaticInfo %s_sStaticStorageInfo[] = {\n" class_name |> output_cpp;
    List.iter dump_static_storage tcpp_class.tcl_static_variables;
    output_cpp "\t{ ::hx::fsUnknown, 0, null()}\n};\n")
  else
    Printf.sprintf "static ::hx::StaticInfo* %s_sStaticStorageInfo = 0;\n" class_name |> output_cpp;

  output_cpp "#endif\n\n";

  (match print_reflective_fields ctx.ctx_common class_def tcpp_class.tcl_variables tcpp_class.tcl_functions tcpp_class.tcl_abstract_functions with
  | Some str ->
    Printf.sprintf "static ::String %s_sMemberFields[] = {\n%s\n};\n\n" class_name str |> output_cpp
  | None ->
    Printf.sprintf "static ::String* %s_sMemberFields = 0;\n\n" class_name |> output_cpp);

  if List.length tcpp_class.tcl_static_variables > 0 then (
    let dump_field_iterator macro field =
      Printf.sprintf "\t%s(%s::%s, \"%s\");" macro class_name (keyword_remap field.cf_name) field.cf_name
    in

    (* Mark static variables as used *)
    let marks =
      tcpp_class.tcl_static_variables
      |> List.map (dump_field_iterator "HX_MARK_MEMBER_NAME")
      |> String.concat "\n" in

    Printf.sprintf "static void %s_sMarkStatics(HX_MARK_PARAMS) { \n%s\n };\n\n" class_name marks |> output_cpp;

    (* Visit static variables *)
    let visits =
      tcpp_class.tcl_static_variables
      |> List.map (dump_field_iterator "HX_VISIT_MEMBER_NAME")
      |> String.concat "\n" in

    output_cpp "#ifdef HXCPP_VISIT_ALLOCS\n";
    Printf.sprintf "static void %s_sVisitStatics(HX_VISIT_PARAMS) { \n%s\n };\n\n" class_name visits |> output_cpp;
    output_cpp "#endif\n\n");

  let generate_script_function isStatic field scriptName callName =
    match follow field.cf_type with
    | TFun (args, return_type) when not (is_data_member field) ->
        let isTemplated = not isStatic in
        if isTemplated then output_cpp "\ntemplate<bool _HX_SUPER=false>";
        output_cpp
          ("\nstatic void CPPIA_CALL " ^ scriptName
         ^ "(::hx::CppiaCtx *ctx) {\n");
        let ret =
          match cpp_type_of return_type with
          | TCppScalar "bool" -> "b"
          | _ -> CppCppia.script_signature return_type false
        in
        if ret <> "v" then
          output_cpp
            ("ctx->return" ^ CppCppia.script_type return_type false ^ "(");

        let dump_call cast =
          if isStatic then
            output_cpp (class_name ^ "::" ^ callName ^ "(")
          else
            output_cpp
              ("((" ^ class_name ^ "*)ctx->getThis())->" ^ cast ^ callName ^ "(");

          let signature, _, _ =
            List.fold_left
              (fun (signature, sep, size) (_, opt, t) ->
                output_cpp
                  (sep ^ "ctx->get" ^ CppCppia.script_type t opt ^ "(" ^ size
                 ^ ")");
                ( signature ^ CppCppia.script_signature t opt,
                  ",",
                  size ^ "+sizeof(" ^ CppCppia.script_size_type t opt ^ ")" ))
              (ret, "", "sizeof(void*)") args
          in
          output_cpp ")";
          signature
        in
        let signature =
          if isTemplated then (
            output_cpp " _HX_SUPER ? ";
            ignore (dump_call (class_name ^ "::"));
            output_cpp " : ";
            dump_call "")
          else dump_call ""
        in

        if ret <> "v" then output_cpp ")";
        output_cpp ";\n}\n";
        signature
    | _ -> ""
  in

  if scriptable then (
    let dump_script_field idx (field, f_args, return_t) =
      let args = print_tfun_arg_list true f_args in
      let names = List.map (fun (n, _, _) -> keyword_remap n) f_args in
      let return_type = type_to_string return_t in
      let ret =
        if return_type = "Void" || return_type = "void" then " " else "return "
      in
      let name = keyword_remap field.cf_name in
      let vtable = "__scriptVTable[" ^ string_of_int (idx + 1) ^ "] " in

      output_cpp ("\t" ^ return_type ^ " " ^ name ^ "( " ^ args ^ " ) {\n");
      output_cpp ("\tif (" ^ vtable ^ ") {\n");
      output_cpp "\t\t::hx::CppiaCtx *__ctx = ::hx::CppiaCtx::getCurrent();\n";
      output_cpp "\t\t::hx::AutoStack __as(__ctx);\n";
      output_cpp
        ("\t\t__ctx->pushObject( this );\n");
      List.iter
        (fun (name, opt, t) ->
          output_cpp
            ("\t\t__ctx->push" ^ CppCppia.script_type t opt ^ "("
            ^ keyword_remap name ^ ");\n"))
        f_args;
      output_cpp
        ("\t\t" ^ ret ^ "__ctx->run"
        ^ CppCppia.script_type return_t false
        ^ "(" ^ vtable ^ ");\n");
      output_cpp ("\t}  else " ^ ret);

      output_cpp
        (class_name ^ "::" ^ name ^ "(" ^ String.concat "," names ^ ");");
      if return_type <> "void" then output_cpp "return null();";
      output_cpp "}\n";
    in

    let new_sctipt_functions = List.rev (current_virtual_functions_rev class_def []) in
    let sctipt_name = class_name ^ "__scriptable" in

    output_cpp ("class " ^ sctipt_name ^ " : public " ^ class_name ^ " {\n");
    output_cpp ("   typedef " ^ sctipt_name ^ " __ME;\n");
    output_cpp ("   typedef " ^ class_name ^ " super;\n");
    let field_arg_count field =
      match (follow field.cf_type, field.cf_kind) with
      | _, Method MethDynamic -> -1
      | TFun (args, return_type), Method _ -> List.length args
      | _, _ -> -1
    in
    let has_funky_toString =
      List.exists
        (fun f -> f.cf_name = "toString")
        class_def.cl_ordered_statics
      || List.exists
            (fun f -> f.cf_name = "toString" && field_arg_count f <> 0)
            class_def.cl_ordered_fields
    in
    let super_string =
      if has_funky_toString then class_name ^ "::super" else class_name
    in
    output_cpp ("   typedef " ^ super_string ^ " __superString;\n");
    output_cpp
      ("   HX_DEFINE_SCRIPTABLE(HX_ARR_LIST"
      ^ string_of_int (List.length constructor_var_list)
      ^ ")\n");
    output_cpp "\tHX_DEFINE_SCRIPTABLE_DYNAMIC;\n";

    let list_iteri func in_list =
      let idx = ref 0 in
      List.iter
        (fun elem ->
          func !idx elem;
          idx := !idx + 1)
        in_list
    in

    let not_toString (field, args, _) = field.cf_name <> "toString" in
    let functions = List.filter not_toString (all_virtual_functions class_def) in
    list_iteri dump_script_field functions;
    output_cpp "};\n\n";

    let sigs = Hashtbl.create 0 in

    let static_functions =
      List.filter (fun f -> not (is_data_member f)) reflect_static_fields
    in
    let all_script_functions =
      List.map (fun (f, _, _) -> f) new_sctipt_functions @ static_functions
    in

    if List.length all_script_functions > 0 then (
      List.iter
        (fun (f, _, _) ->
          let s =
            generate_script_function false f ("__s_" ^ f.cf_name)
              (keyword_remap f.cf_name)
          in
          Hashtbl.add sigs f.cf_name s)
        new_sctipt_functions;

      let dump_script_static f =
        let s =
          generate_script_function true f ("__s_" ^ f.cf_name)
            (keyword_remap f.cf_name)
        in
        Hashtbl.add sigs f.cf_name s
      in
      List.iter dump_script_static class_def.cl_ordered_statics;

      output_cpp "#ifndef HXCPP_CPPIA_SUPER_ARG\n";
      output_cpp "#define HXCPP_CPPIA_SUPER_ARG(x)\n";
      output_cpp "#endif\n";
      output_cpp
        "static ::hx::ScriptNamedFunction __scriptableFunctions[] = {\n";
      let dump_func f isStaticFlag =
        let s = try Hashtbl.find sigs f.cf_name with Not_found -> "v" in
        output_cpp
          ("  ::hx::ScriptNamedFunction(\"" ^ f.cf_name ^ "\",__s_" ^ f.cf_name
         ^ ",\"" ^ s ^ "\", " ^ isStaticFlag ^ " ");
        let superCall =
          if isStaticFlag = "true" then
            "0"
          else
            "__s_" ^ f.cf_name ^ "<true>"
        in
        output_cpp ("HXCPP_CPPIA_SUPER_ARG(" ^ superCall ^ ")");
        output_cpp " ),\n"
      in
      List.iter (fun (f, _, _) -> dump_func f "false") new_sctipt_functions;
      List.iter (fun f -> dump_func f "true") static_functions;
      output_cpp
        "  ::hx::ScriptNamedFunction(0,0,0 HXCPP_CPPIA_SUPER_ARG(0) ) };\n")
    else
      output_cpp
        "static ::hx::ScriptNamedFunction *__scriptableFunctions = 0;\n";);

  let class_name_text = join_class_path class_path "." in

  (* Initialise static in boot function ... *)
  (* Remap the specialised "extern" classes back to the generic names *)
  output_cpp ("::hx::Class " ^ class_name ^ "::__mClass;\n\n");
  (if scriptable then
      match class_def.cl_constructor with
      | Some field ->
          let signature =
            generate_script_function false field "__script_construct_func"
              "__construct"
          in
          output_cpp
            ("::hx::ScriptFunction " ^ class_name
          ^ "::__script_construct(__script_construct_func,\"" ^ signature
          ^ "\");\n")
      | _ ->
          output_cpp
            ("::hx::ScriptFunction " ^ class_name
          ^ "::__script_construct(0,0);\n"));

  (match print_reflective_fields ctx.ctx_common class_def tcpp_class.tcl_static_variables tcpp_class.tcl_static_functions [] with
  | Some str ->
    Printf.sprintf "static ::String %s_sStaticFields[] = {\n%s\n};\n\n" class_name str |> output_cpp
  | None ->
    Printf.sprintf "static ::String* %s_sStaticFields = 0;\n\n" class_name |> output_cpp);

  output_cpp ("void " ^ class_name ^ "::__register()\n{\n");
  if not (has_class_flag class_def CAbstract) then (
    output_cpp ("\t" ^ class_name ^ " _hx_dummy;\n");
    output_cpp ("\t" ^ class_name ^ "::_hx_vtable = *(void **)&_hx_dummy;\n"));
  output_cpp "\t::hx::Static(__mClass) = new ::hx::Class_obj();\n";
  output_cpp ("\t__mClass->mName = " ^ strq class_name_text ^ ";\n");
  output_cpp "\t__mClass->mSuper = &super::__SGetClass();\n";
  if not (has_class_flag class_def CAbstract) then (
    output_cpp "\t__mClass->mConstructEmpty = &__CreateEmpty;\n";
    output_cpp "\t__mClass->mConstructArgs = &__Create;\n");
  output_cpp
    ("\t__mClass->mGetStaticField = &"
    ^
    if has_get_static_field class_def then class_name ^ "::__GetStatic;\n"
    else "::hx::Class_obj::GetNoStaticField;\n");
  output_cpp
    ("\t__mClass->mSetStaticField = &"
    ^
    if has_set_static_field class_def then class_name ^ "::__SetStatic;\n"
    else "::hx::Class_obj::SetNoStaticField;\n");
  if List.length tcpp_class.tcl_static_variables > 0 then
    output_cpp ("\t__mClass->mMarkFunc = " ^ class_name ^ "_sMarkStatics;\n");
  Printf.sprintf
    "\t__mClass->mStatics = ::hx::Class_obj::dupFunctions(%s_sStaticFields);\n" class_name |> output_cpp;
  Printf.sprintf
    "\t__mClass->mMembers = ::hx::Class_obj::dupFunctions(%s_sMemberFields);\n" class_name |> output_cpp;
  output_cpp ("\t__mClass->mCanCast = ::hx::TCanCast< " ^ class_name ^ " >;\n");
  if List.length tcpp_class.tcl_static_variables > 0 then
    output_cpp
      ("#ifdef HXCPP_VISIT_ALLOCS\n\t__mClass->mVisitFunc = " ^ class_name
      ^ "_sVisitStatics;\n#endif\n");
  output_cpp
    ("#ifdef HXCPP_SCRIPTABLE\n\t__mClass->mMemberStorageInfo = " ^ class_name
    ^ "_sMemberStorageInfo;\n#endif\n");
  output_cpp
    ("#ifdef HXCPP_SCRIPTABLE\n\t__mClass->mStaticStorageInfo = " ^ class_name
    ^ "_sStaticStorageInfo;\n#endif\n");
  output_cpp "\t::hx::_hx_RegisterClass(__mClass->mName, __mClass);\n";
  if scriptable then
    output_cpp
      ("  HX_SCRIPTABLE_REGISTER_CLASS(\"" ^ class_name_text ^ "\","
      ^ class_name ^ ");\n");
  List.iter
    (fun intf_def ->
      output_cpp
        ("\tHX_REGISTER_VTABLE_OFFSET( " ^ class_name ^ ","
        ^ join_class_path_remap intf_def.cl_path "::"
        ^ ");\n"))
        tcpp_class.tcl_native_parents;
  output_cpp "}\n\n";

  gen_boot_field ctx output_cpp tcpp_class;

  end_namespace output_cpp class_path;

  cpp_file#close