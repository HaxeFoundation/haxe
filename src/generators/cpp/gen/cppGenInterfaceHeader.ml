open Ast
open Type
open Common
open CppStrings
open CppTypeUtils
open CppAst
open CppAstTools
open CppSourceWriter
open CppContext
open CppGen

let attribs common_ctx = match Gctx.defined common_ctx Define.DllExport with
  | true -> "HXCPP_EXTERN_CLASS_ATTRIBUTES"
  | false -> "HXCPP_CLASS_ATTRIBUTES"

let gen_native_function ctx interface func =
  let output   = ctx.ctx_output in
  let gen_args = print_retyped_tfun_arg_list true in
  let strq     = strq ctx.ctx_common in

  Printf.sprintf "\t\tvirtual %s %s(%s)=0;\n" (tcpp_to_string func.iff_return) func.iff_name (gen_args func.iff_args) |> output;
  if reflective interface.if_class func.iff_field then
    if Gctx.defined ctx.ctx_common Define.DynamicInterfaceClosures then
      Printf.sprintf
        "\t\tinline ::Dynamic %s_dyn() { return __Field( %s, ::hx::paccDynamic ); }\n"
        func.iff_name
        (strq func.iff_field.cf_name) |> output
    else
      Printf.sprintf "\t\tvirtual ::Dynamic %s_dyn()=0;\n" func.iff_name |> output

let gen_function ctx interface func =
  let output       = ctx.ctx_output in
  let argList      = print_retyped_tfun_arg_list true func.iff_args in
  let returnType   = match func.iff_return with
  | TCppMarshalNativeType (value_type, (Reference | Promoted)) ->
    TCppMarshalNativeType (value_type, Stack) |> tcpp_to_string
  | other ->
    tcpp_to_string other
  in
  let commaArgList = if argList = "" then argList else "," ^ argList in

  Printf.sprintf "\t\t%s (::hx::Object :: *_hx_%s)(%s);\n" returnType func.iff_name argList |> output;
  Printf.sprintf "\t\tstatic %s %s( ::Dynamic _hx_%s );\n" returnType func.iff_name commaArgList |> output

let gen_includes h_file interface_def =
  let add_class_includes cls =
    match get_all_meta_string_path cls.cl_meta Meta.Include with
    | [] ->
      h_file#add_include cls.cl_path
    | includes ->
      List.iter (fun inc -> h_file#add_include (path_of_string inc)) includes in

  (* Include the real header file for the super class *)
  match interface_def.cl_super with
  | Some (cls, _) -> add_class_includes cls
  | _ -> ();

  (* And any interfaces ... *)
  interface_def.cl_implements
    |> real_interfaces
    |> List.iter (fun (cls, _) -> add_class_includes cls)

let gen_forward_decls h_file tcpp_interface ctx common_ctx =
  (* Only need to forward-declare classes that are mentioned in the header file (ie, not the implementation) *)
  let header_referenced, header_flags =
    CppReferences.find_referenced_types_flags ctx (TClassDecl tcpp_interface.if_class) None ctx.ctx_super_deps PathMap.empty true false tcpp_interface.if_scriptable
  in

  List.iter2
    (fun r f -> gen_forward_decl h_file r f)
    header_referenced header_flags

let gen_header_includes interface_def output_h =
  output_h "\n";
  output_h (get_class_code interface_def Meta.HeaderCode);
  let includes = get_all_meta_string_path interface_def.cl_meta Meta.HeaderInclude in
  let printer inc = output_h ("#include \"" ^ inc ^ "\"\n") in
  List.iter printer includes

let gen_body tcpp_interface ctx output_h iter =
  if has_boot_field tcpp_interface.if_class then output_h "\t\tstatic void __boot();\n";

  all_interface_functions tcpp_interface |> List.iter iter;

  match get_meta_string tcpp_interface.if_class.cl_meta Meta.ObjcProtocol with
  | Some protocol ->
    output_h ("\t\tstatic id<" ^ protocol ^ "> _hx_toProtocol(::Dynamic inImplementation);\n")
  | None ->
    ();

  output_h (get_class_code tcpp_interface.if_class Meta.HeaderClassCode)

let generate_native_interface base_ctx tcpp_interface =
  let common_ctx = base_ctx.ctx_common in
  let class_path = tcpp_interface.if_class.cl_path in

  let parent, super =
    match tcpp_interface.if_class.cl_super with
    | Some (klass, params) ->
      let name = tcpp_to_string_suffix "_obj" (cpp_instance_type common_ctx.basic klass params CppRetyper.with_stack_value_type) in
      ( "virtual " ^ name, name )
    | None ->
      ("virtual ::hx::NativeInterface", "::hx::NativeInterface")
  in

  let h_file     = new_header_file common_ctx common_ctx.file class_path in
  let ctx        = file_context base_ctx h_file tcpp_interface.if_debug_level true in
  let output_h   = h_file#write in
  let def_string = join_class_path class_path "_" in

  begin_header_file h_file#write_h def_string true;

  gen_includes h_file tcpp_interface.if_class;
  gen_forward_decls h_file tcpp_interface ctx common_ctx;
  gen_header_includes tcpp_interface.if_class output_h;

  begin_namespace output_h class_path;
  output_h "\n\n";
  output_h (get_class_code tcpp_interface.if_class Meta.HeaderNamespaceCode);

  output_h ("class " ^ (attribs common_ctx) ^ " " ^ tcpp_interface.if_name ^ " : public " ^ parent);
  
  tcpp_interface.if_class.cl_implements
    |> List.filter (fun (t, _) -> is_native_gen_class t)
    |> List.iter (fun (c, _) -> output_h (" , public virtual " ^ join_class_path c.cl_path "::"));

  output_h "\n{\n\tpublic:\n";
  output_h ("\t\ttypedef " ^ super ^ " super;\n");
  output_h ("\t\ttypedef " ^ tcpp_interface.if_name ^ " OBJ_;\n");

  gen_body tcpp_interface ctx output_h (gen_native_function ctx tcpp_interface);
  
  output_h "};\n\n";

  end_namespace output_h class_path;
  end_header_file output_h def_string;

  h_file#close

let generate_managed_interface base_ctx tcpp_interface =
  let common_ctx = base_ctx.ctx_common in
  let class_path = tcpp_interface.if_class.cl_path in

  let super =
    match tcpp_interface.if_class.cl_super with
    | Some (klass, params) ->
      tcpp_to_string_suffix "_obj" (cpp_instance_type common_ctx.basic klass params CppRetyper.with_stack_value_type)
    | None ->
      "::hx::Object"
  in
  let h_file     = new_header_file common_ctx common_ctx.file class_path in
  let ctx        = file_context base_ctx h_file tcpp_interface.if_debug_level true in
  let output_h   = h_file#write in
  let def_string = join_class_path class_path "_" in

  begin_header_file h_file#write_h def_string false;

  gen_includes h_file tcpp_interface.if_class;
  gen_forward_decls h_file tcpp_interface ctx common_ctx;
  gen_header_includes tcpp_interface.if_class output_h;

  begin_namespace output_h class_path;
  output_h "\n\n";
  output_h (get_class_code tcpp_interface.if_class Meta.HeaderNamespaceCode);

  output_h ("class " ^ (attribs common_ctx) ^ " " ^ tcpp_interface.if_name ^ " {\n");
  output_h "\tpublic:\n";
  output_h ("\t\ttypedef " ^ super ^ " super;\n");
  output_h "\t\tHX_DO_INTERFACE_RTTI;\n\n";

  gen_body tcpp_interface ctx output_h (gen_function ctx tcpp_interface);
  
  output_h "};\n\n";

  end_namespace output_h class_path;
  end_header_file output_h def_string;

  h_file#close