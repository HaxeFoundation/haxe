open Type
open CppStrings
open CppAst
open CppAstTools
open CppSourceWriter
open CppContext
open CppGen

let constructor_arg_count constructor =
  match constructor.ef_type with
  | TFun(args, _) -> List.length args
  | _ -> 0

let gen_enum_constructor remap_class_name class_name output_cpp constructor =
  match constructor.tef_field.ef_type with
  | TFun (args, _) ->
    Printf.sprintf "%s %s::%s(%s)\n" remap_class_name class_name constructor.tef_name (print_tfun_arg_list true args) |> output_cpp;
    Printf.sprintf "{\n\treturn ::hx::CreateEnum<%s>(%s,%i,%i)" class_name constructor.tef_hash constructor.tef_field.ef_index (List.length args) |> output_cpp;

    args
      |> List.mapi (fun i (arg, _, _) -> Printf.sprintf "->_hx_init(%i,%s)" i (keyword_remap arg))
      |> List.iter output_cpp;

    output_cpp ";\n}\n\n"
  | _ ->
    output_cpp ( remap_class_name ^ " " ^ class_name ^ "::" ^ constructor.tef_name ^ ";\n\n" )

let gen_static_reflection class_name output_cpp constructor =
  let dyn = if constructor_arg_count constructor.tef_field > 0 then "_dyn()" else "" in
  Printf.sprintf "\tif (inName==%s) { outValue = %s::%s%s; return true; }\n" constructor.tef_hash class_name constructor.tef_name dyn |> output_cpp

let gen_dynamic_constructor class_name output_cpp constructor =
  let count = constructor_arg_count constructor.tef_field in
  if (count>0) then begin
    Printf.sprintf "STATIC_HX_DEFINE_DYNAMIC_FUNC%i(%s, %s, return)\n\n" count class_name constructor.tef_name |> output_cpp;
  end

let generate base_ctx tcpp_enum =
  let common_ctx       = base_ctx.ctx_common in
  let class_path       = tcpp_enum.te_enum.e_path in
  let just_class_name  = (snd class_path) in
  let class_name       = just_class_name ^ "_obj" in
  let remap_class_name = ("::" ^ (join_class_path_remap class_path "::") )  in
  let cpp_file         = new_placed_cpp_file common_ctx class_path in
  let output_cpp       = (cpp_file#write) in
  let debug            = if (Meta.has Meta.NoDebug tcpp_enum.te_enum.e_meta) || ( Gctx.defined common_ctx Define.NoDebug) then 0 else 1 in

  let ctx = file_context base_ctx cpp_file debug false in
  let strq = strq ctx.ctx_common in
  let classIdTxt = Printf.sprintf "0x%08lx" tcpp_enum.te_id in

  if (debug>1) then
    print_endline ("Found enum definition:" ^ (join_class_path  class_path "::" ));

  cpp_file#write_h "#include <hxcpp.h>\n\n";

  let referenced,flags = CppReferences.find_referenced_types_flags ctx (TEnumDecl tcpp_enum.te_enum) None ctx.ctx_super_deps PathMap.empty false false false in
  List.iter (add_include cpp_file) referenced;

  begin_namespace output_cpp class_path;
  output_cpp "\n";

  List.iter (gen_enum_constructor remap_class_name class_name output_cpp) tcpp_enum.te_constructors;

  output_cpp ("bool " ^ class_name ^ "::__GetStatic(const ::String &inName, ::Dynamic &outValue, ::hx::PropertyAccess inCallProp)\n{\n");
  List.iter (gen_static_reflection class_name output_cpp) tcpp_enum.te_constructors;
  output_cpp ("\treturn super::__GetStatic(inName, outValue, inCallProp);\n}\n\n");

  output_cpp ("HX_DEFINE_CREATE_ENUM(" ^ class_name ^ ")\n\n");

  output_cpp ("bool " ^ class_name ^ "::_hx_isInstanceOf(int inClassId) {\n");
  output_cpp ("\treturn inClassId == (int)0x00000001 || inClassId == ::hx::EnumBase_obj::_hx_ClassId || inClassId == _hx_ClassId;\n");
  output_cpp ("}\n");

  output_cpp ("int " ^ class_name ^ "::__FindIndex(::String inName)\n{\n");
  List.iter
    (fun constructor -> Printf.sprintf "\tif (inName==%s) return %i;\n" constructor.tef_hash constructor.tef_field.ef_index |> output_cpp)
    tcpp_enum.te_constructors;
  output_cpp ("\treturn super::__FindIndex(inName);\n");
  output_cpp ("}\n\n");

  (* Dynamic versions of constructors *)
  List.iter (gen_dynamic_constructor class_name output_cpp) tcpp_enum.te_constructors;

  output_cpp ("int " ^ class_name ^ "::__FindArgCount(::String inName)\n{\n");
  List.iter
    (fun constructor -> Printf.sprintf "\tif (inName==%s) return %i;\n" constructor.tef_hash (constructor_arg_count constructor.tef_field) |> output_cpp)
    tcpp_enum.te_constructors;

  output_cpp ("\treturn super::__FindArgCount(inName);\n");
  output_cpp ("}\n\n");

  (* Dynamic "Get" Field function - string version *)
  output_cpp ("::hx::Val " ^ class_name ^ "::__Field(const ::String &inName,::hx::PropertyAccess inCallProp)\n{\n");
  let dump_constructor_test constructor =
      output_cpp ("\tif (inName==" ^ constructor.tef_hash ^ ") return " ^ constructor.tef_name );
      if ( (constructor_arg_count constructor.tef_field) > 0 ) then output_cpp "_dyn()";
      output_cpp (";\n")
  in
  List.iter dump_constructor_test tcpp_enum.te_constructors;
  output_cpp ("\treturn super::__Field(inName,inCallProp);\n}\n\n");

  output_cpp ("static ::String " ^ class_name ^ "_sStaticFields[] = {\n");
  List.iter (fun constructor -> output_cpp ("\t" ^ constructor.tef_hash ^ ",\n") ) tcpp_enum.te_constructors;

  output_cpp "\t::String(null())\n};\n\n";

  (* ENUM - Mark static as used by GC - they are const now, so no marking*)
  (* ENUM - Visit static as used by GC - none *)

  output_cpp ("::hx::Class " ^ class_name ^ "::__mClass;\n\n");

  output_cpp ("::Dynamic __Create_" ^ class_name ^ "() { return new " ^ class_name ^ "; }\n\n");

  output_cpp ("void " ^ class_name ^ "::__register()\n{\n");
  let text_name = strq (join_class_path class_path ".") in
  output_cpp ("\n::hx::Static(__mClass) = ::hx::_hx_RegisterClass(" ^ text_name ^
              ", ::hx::TCanCast< " ^ class_name ^ " >," ^ class_name ^ "_sStaticFields,0,\n");
  output_cpp ("\t&__Create_" ^ class_name ^ ", &__Create,\n");
  output_cpp ("\t&super::__SGetClass(), &Create" ^ class_name ^ ", 0\n");
  output_cpp("#ifdef HXCPP_VISIT_ALLOCS\n    , 0\n#endif\n");
  output_cpp ("#ifdef HXCPP_SCRIPTABLE\n    , 0\n#endif\n");
      output_cpp (");\n");
  output_cpp ("\t__mClass->mGetStaticField = &" ^ class_name ^"::__GetStatic;\n");
  output_cpp "}\n\n";

  output_cpp ("void " ^ class_name ^ "::__boot()\n{\n");
  (match Texpr.build_metadata common_ctx.basic (TEnumDecl tcpp_enum.te_enum) with
  | Some expr ->
    let ctx = file_context ctx cpp_file 1 false in
    gen_cpp_init ctx class_name "boot" "__mClass->__meta__ = " expr
  | _ -> () );

  List.iter
    (fun constructor ->
      match constructor.tef_field.ef_type with
      | TFun (_,_) ->
        ()
      | _ ->
        Printf.sprintf "%s = ::hx::CreateConstEnum<%s>(%s, %i);\n" constructor.tef_name class_name constructor.tef_hash constructor.tef_field.ef_index |> output_cpp)
    tcpp_enum.te_constructors;

  output_cpp ("}\n\n");

  output_cpp "\n";
  end_namespace output_cpp class_path;
  cpp_file#close;

  let h_file = new_header_file common_ctx common_ctx.file class_path in
  let super = "::hx::EnumBase_obj" in
  let output_h = (h_file#write) in
  let def_string = join_class_path class_path "_"  in

  begin_header_file (h_file#write_h) def_string false;

  List.iter2 (fun r f -> gen_forward_decl h_file r f) referenced flags;

  output_h ( get_code tcpp_enum.te_enum.e_meta Meta.HeaderCode );

  begin_namespace output_h class_path;

  output_h "\n\n";
  output_h ("class " ^ class_name ^ " : public " ^ super ^ "\n");
  output_h ("{\n\ttypedef " ^ super ^ " super;\n");
  output_h ("\t\ttypedef " ^ class_name ^ " OBJ_;\n");
  output_h "\n\tpublic:\n";
  output_h ("\t\tenum { _hx_ClassId = " ^ classIdTxt ^ " };\n\n");
  output_h ("\t\t" ^ class_name ^ "() {};\n");
  output_h ("\t\tHX_DO_ENUM_RTTI;\n");
  output_h ("\t\tstatic void __boot();\n");
  output_h ("\t\tstatic void __register();\n");
  output_h ("\t\tstatic bool __GetStatic(const ::String &inName, ::Dynamic &outValue, ::hx::PropertyAccess inCallProp);\n");
  output_h ("\t\t::String GetEnumName( ) const { return " ^ (strq (join_class_path class_path "."))  ^ "; }\n" );
  output_h ("\t\t::String __ToString() const { return " ^ (strq (just_class_name ^ ".") )^ " + _hx_tag; }\n");
  output_h ("\t\tbool _hx_isInstanceOf(int inClassId);\n\n");

  List.iter
    (fun constructor ->
      Printf.sprintf "\t\tstatic %s %s" remap_class_name constructor.tef_name |> output_h;
      match constructor.tef_field.ef_type with
      | TFun (args,_) ->
        Printf.sprintf "(%s);\n" (print_tfun_arg_list true args) |> output_h;
        Printf.sprintf "\t\tstatic ::Dynamic %s_dyn();\n" constructor.tef_name |> output_h;
      | _ ->
        output_h ";\n";
        Printf.sprintf "\t\tstatic inline %s %s_dyn() { return %s; }\n" remap_class_name constructor.tef_name constructor.tef_name |> output_h;)
    tcpp_enum.te_constructors;

  output_h "};\n\n";

  end_namespace output_h class_path;

  end_header_file output_h def_string;
  h_file#close
