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

let generate baseCtx enum_def =
  let common_ctx       = baseCtx.ctx_common in
  let class_path       = enum_def.e_path in
  let just_class_name  = (snd class_path) in
  let class_name       = just_class_name ^ "_obj" in
  let remap_class_name = ("::" ^ (join_class_path_remap class_path "::") )  in
  let cpp_file         = new_placed_cpp_file common_ctx class_path in
  let output_cpp       = (cpp_file#write) in
  let debug            = if (Meta.has Meta.NoDebug enum_def.e_meta) || ( Common.defined common_ctx Define.NoDebug) then 0 else 1 in

  let ctx = file_context baseCtx cpp_file debug false in
  let strq = strq ctx.ctx_common in

  let classId = try Hashtbl.find baseCtx.ctx_type_ids (class_text enum_def.e_path) with Not_found -> Int32.zero in
  let classIdTxt = Printf.sprintf "0x%08lx" classId in

  if (debug>1) then
      print_endline ("Found enum definition:" ^ (join_class_path  class_path "::" ));

  cpp_file#write_h "#include <hxcpp.h>\n\n";

  let super_deps       = create_super_dependencies common_ctx in
  let referenced,flags = CppReferences.find_referenced_types_flags ctx (TEnumDecl enum_def) "*" super_deps (Hashtbl.create 0) false false false in
  List.iter (add_include cpp_file) referenced;

  begin_namespace output_cpp class_path;
  output_cpp "\n";

  PMap.iter (fun _ constructor ->
      let name = keyword_remap constructor.ef_name in
      match constructor.ef_type with
      | TFun (args,_) ->
        output_cpp (remap_class_name ^ " " ^ class_name ^ "::" ^ name ^ "(" ^
            (print_tfun_arg_list true args) ^")\n");

        output_cpp ("{\n\treturn ::hx::CreateEnum< " ^ class_name ^ " >(" ^ (strq name) ^ "," ^
            (string_of_int constructor.ef_index) ^ "," ^ (string_of_int (List.length args)) ^  ")" );
          ExtList.List.iteri (fun i (arg,_,_) -> output_cpp ("->_hx_init(" ^ (string_of_int i) ^ "," ^ (keyword_remap arg) ^ ")")) args;
        output_cpp ";\n}\n\n"
      | _ ->
        output_cpp ( remap_class_name ^ " " ^ class_name ^ "::" ^ name ^ ";\n\n" )
  ) enum_def.e_constrs;


  let constructor_arg_count constructor =
      (match constructor.ef_type with | TFun(args,_) -> List.length args | _ -> 0 )
  in

  output_cpp ("bool " ^ class_name ^ "::__GetStatic(const ::String &inName, ::Dynamic &outValue, ::hx::PropertyAccess inCallProp)\n{\n");
  PMap.iter (fun _ constructor ->
      let name = constructor.ef_name in
      let dyn = if constructor_arg_count constructor > 0 then "_dyn()" else "" in
      output_cpp ("\tif (inName==" ^ strq name ^ ") { outValue = " ^ class_name ^ "::" ^ keyword_remap name ^ dyn ^ "; return true; }\n" );
  ) enum_def.e_constrs;
  output_cpp ("\treturn super::__GetStatic(inName, outValue, inCallProp);\n}\n\n");

  output_cpp ("HX_DEFINE_CREATE_ENUM(" ^ class_name ^ ")\n\n");

  output_cpp ("bool " ^ class_name ^ "::_hx_isInstanceOf(int inClassId) {\n");
  output_cpp ("\treturn inClassId == (int)0x00000001 || inClassId == ::hx::EnumBase_obj::_hx_ClassId || inClassId == _hx_ClassId;\n");
  output_cpp ("}\n");

  output_cpp ("int " ^ class_name ^ "::__FindIndex(::String inName)\n{\n");
  PMap.iter (fun _ constructor ->
      let name = constructor.ef_name in
      let idx = string_of_int constructor.ef_index in
      output_cpp ("\tif (inName==" ^ (strq name) ^ ") return " ^ idx ^ ";\n") ) enum_def.e_constrs;
  output_cpp ("\treturn super::__FindIndex(inName);\n");
  output_cpp ("}\n\n");

  (* Dynamic versions of constructors *)
  let dump_dynamic_constructor _ constr =
      let count = constructor_arg_count constr in
      if (count>0) then begin
        let nargs = string_of_int count in
        output_cpp ("STATIC_HX_DEFINE_DYNAMIC_FUNC" ^ nargs ^ "(" ^ class_name ^ "," ^
              (keyword_remap constr.ef_name) ^ ",return)\n\n");
      end
  in
  PMap.iter dump_dynamic_constructor enum_def.e_constrs;


  output_cpp ("int " ^ class_name ^ "::__FindArgCount(::String inName)\n{\n");
  PMap.iter (fun _ constructor ->
      let name = constructor.ef_name in
      let count = string_of_int (constructor_arg_count constructor) in
      output_cpp ("\tif (inName==" ^ (strq name) ^ ") return " ^ count ^ ";\n") ) enum_def.e_constrs;
      output_cpp ("\treturn super::__FindArgCount(inName);\n");
      output_cpp ("}\n\n");

  (* Dynamic "Get" Field function - string version *)
  output_cpp ("::hx::Val " ^ class_name ^ "::__Field(const ::String &inName,::hx::PropertyAccess inCallProp)\n{\n");
  let dump_constructor_test _ constr =
      output_cpp ("\tif (inName==" ^ (strq constr.ef_name) ^ ") return " ^
                  (keyword_remap constr.ef_name) );
      if ( (constructor_arg_count constr) > 0 ) then output_cpp "_dyn()";
      output_cpp (";\n")
  in
  PMap.iter dump_constructor_test enum_def.e_constrs;
  output_cpp ("\treturn super::__Field(inName,inCallProp);\n}\n\n");

  output_cpp ("static ::String " ^ class_name ^ "_sStaticFields[] = {\n");
  let sorted =
      List.sort (fun f1 f2 -> (PMap.find f1 enum_def.e_constrs ).ef_index -
              (PMap.find f2 enum_def.e_constrs ).ef_index )
        (pmap_keys enum_def.e_constrs) in

    List.iter (fun name -> output_cpp ("\t" ^ (strq name) ^ ",\n") ) sorted;

  output_cpp "\t::String(null())\n};\n\n";

  (* ENUM - Mark static as used by GC - they are const now, so no marking*)
  (* ENUM - Visit static as used by GC - none *)

  output_cpp ("::hx::Class " ^ class_name ^ "::__mClass;\n\n");

  output_cpp ("Dynamic __Create_" ^ class_name ^ "() { return new " ^ class_name ^ "; }\n\n");

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
  (match Texpr.build_metadata common_ctx.basic (TEnumDecl enum_def) with
      | Some expr ->
        let ctx = file_context ctx cpp_file 1 false in
        gen_cpp_init ctx class_name "boot" "__mClass->__meta__ = " expr
      | _ -> () );
  PMap.iter (fun _ constructor ->
      let name = constructor.ef_name in
      match constructor.ef_type with
      | TFun (_,_) -> ()
      | _ ->
        output_cpp ( (keyword_remap name) ^ " = ::hx::CreateConstEnum< " ^ class_name ^ " >(" ^ (strq name) ^  "," ^
            (string_of_int constructor.ef_index) ^ ");\n" )
  ) enum_def.e_constrs;
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

  output_h ( get_code enum_def.e_meta Meta.HeaderCode );

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
  output_h ("\t\tstatic bool __GetStatic(const ::String &inName, Dynamic &outValue, ::hx::PropertyAccess inCallProp);\n");
  output_h ("\t\t::String GetEnumName( ) const { return " ^ (strq (join_class_path class_path "."))  ^ "; }\n" );
  output_h ("\t\t::String __ToString() const { return " ^ (strq (just_class_name ^ ".") )^ " + _hx_tag; }\n");
  output_h ("\t\tbool _hx_isInstanceOf(int inClassId);\n\n");


  PMap.iter (fun _ constructor ->
      let name = keyword_remap constructor.ef_name in
      output_h ( "\t\tstatic " ^  remap_class_name ^ " " ^ name );
      match constructor.ef_type with
      | TFun (args,_) ->
        output_h ( "(" ^ (print_tfun_arg_list true args) ^");\n");
        output_h ( "\t\tstatic ::Dynamic " ^ name ^ "_dyn();\n");
      | _ ->
        output_h ";\n";
        output_h ( "\t\tstatic inline " ^  remap_class_name ^ " " ^ name ^
                  "_dyn() { return " ^name ^ "; }\n" );
  ) enum_def.e_constrs;

  output_h "};\n\n";

  end_namespace output_h class_path;

  end_header_file output_h def_string;
  h_file#close 
