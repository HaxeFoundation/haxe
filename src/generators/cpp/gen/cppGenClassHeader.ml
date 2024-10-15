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

let gen_member_def ctx class_def is_static is_interface field =
  let output = ctx.ctx_output in
  let remap_name = keyword_remap field.cf_name in
  let nativeGen = Meta.has Meta.NativeGen class_def.cl_meta in

  if is_interface then
    match (follow field.cf_type, field.cf_kind) with
    | _, Method MethDynamic -> ()
    | TFun (args, return_type), Method _ ->
        let gen_args = print_tfun_arg_list true in
        if is_static || nativeGen then (
          output
            ((if not is_static then "\t\tvirtual " else "\t\t")
            ^ type_to_string return_type);
          output (" " ^ remap_name ^ "( ");
          output (gen_args args);
          output (if not is_static then ")=0;\n" else ");\n");
          if reflective class_def field then
            if Common.defined ctx.ctx_common Define.DynamicInterfaceClosures
            then
              output
                ("\t\tinline ::Dynamic " ^ remap_name
               ^ "_dyn() { return __Field( "
                ^ strq ctx.ctx_common field.cf_name
                ^ ", ::hx::paccDynamic); }\n")
            else output ("\t\tvirtual ::Dynamic " ^ remap_name ^ "_dyn()=0;\n"))
        else
          let argList = gen_args args in
          let returnType = type_to_string return_type in
          let returnStr = if returnType = "void" then "" else "return " in
          let commaArgList = if argList = "" then argList else "," ^ argList in
          let cast =
            "::hx::interface_cast< ::"
            ^ join_class_path_remap class_def.cl_path "::"
            ^ "_obj *>"
          in
          output
            ("\t\t" ^ returnType ^ " (::hx::Object :: *_hx_" ^ remap_name ^ ")("
           ^ argList ^ "); \n");
          output
            ("\t\tstatic inline " ^ returnType ^ " " ^ remap_name
           ^ "( ::Dynamic _hx_" ^ commaArgList ^ ") {\n");
          output "\t\t\t#ifdef HXCPP_CHECK_POINTER\n";
          output
            "\t\t\tif (::hx::IsNull(_hx_)) ::hx::NullReference(\"Object\", \
             false);\n";
          output "\t\t\t#ifdef HXCPP_GC_CHECK_POINTER\n";
          output "\t\t\t\tGCCheckPointer(_hx_.mPtr);\n";
          output "\t\t\t#endif\n";
          output "\t\t\t#endif\n";
          output
            ("\t\t\t" ^ returnStr ^ "(_hx_.mPtr->*( " ^ cast
           ^ "(_hx_.mPtr->_hx_getInterface(" ^ cpp_class_hash class_def
           ^ ")))->_hx_" ^ remap_name ^ ")(" ^ print_arg_names args
           ^ ");\n\t\t}\n")
    | _ -> ()
  else
    let nonVirtual = Meta.has Meta.NonVirtual field.cf_meta in
    let doDynamic =
      (nonVirtual || not (is_override field)) && reflective class_def field
    in
    let decl = get_meta_string field.cf_meta Meta.Decl in
    let has_decl = match decl with Some _ -> true | None -> false in
    if has_decl then output ("      typedef " ^ (decl |> Option.get) ^ ";\n");
    output (if is_static then "\t\tstatic " else "\t\t");
    match field.cf_expr with
    | Some { eexpr = TFunction function_def } ->
        (if is_dynamic_haxe_method field then (
           if doDynamic then (
             output ("::Dynamic " ^ remap_name ^ ";\n");
             if (not is_static) && is_gc_element ctx TCppDynamic then
               output
                 ("\t\tinline ::Dynamic _hx_set_" ^ remap_name
                ^ "(::hx::StackContext *_hx_ctx,::Dynamic _hx_v) { \
                   HX_OBJ_WB(this,_hx_v.mPtr) return " ^ remap_name
                ^ "=_hx_v; }\n");
             output (if is_static then "\t\tstatic " else "\t\t");
             output
               ("inline ::Dynamic &" ^ remap_name ^ "_dyn() " ^ "{return "
              ^ remap_name ^ "; }\n")))
         else
           let return_type = type_to_string function_def.tf_type in
           (if (not is_static) && not nonVirtual then
              let scriptable =
                Common.defined ctx.ctx_common Define.Scriptable
              in
              if (not (is_internal_member field.cf_name)) && not scriptable then
                let key =
                  join_class_path class_def.cl_path "." ^ "." ^ field.cf_name
                in
                try output (Hashtbl.find ctx.ctx_class_member_types key)
                with Not_found -> ()
              else output "virtual ");
           output (if return_type = "Void" then "void" else return_type);

           let remap_name = native_field_name_remap is_static field in
           output (" " ^ remap_name ^ "(");
           output (print_arg_list function_def.tf_args "");
           output ");\n";
           if doDynamic then (
             output (if is_static then "\t\tstatic " else "\t\t");
             output ("::Dynamic " ^ remap_name ^ "_dyn();\n")));
        output "\n"
    | _ when has_class_field_flag field CfAbstract ->
        let ctx_arg_list ctx arg_list prefix =
          let get_default_value name =
            try
              match Meta.get Meta.Value field.cf_meta with
              | _, [ (EObjectDecl decls, _) ], _ ->
                  Some
                    (List.find (fun ((n, _, _), _) -> n = name) decls
                    |> snd
                    |> type_constant_value ctx.ctx_common.basic)
              | _ -> None
            with Not_found -> None
          in

          String.concat ","
            (List.map
               (fun (n, o, t) -> print_arg n (get_default_value n) t prefix)
               arg_list)
        in
        let tl, tr =
          match follow field.cf_type with
          | TFun (tl, tr) -> (tl, tr)
          | _ -> die "" __LOC__
        in
        let return_type = type_to_string tr in
        let remap_name = native_field_name_remap is_static field in
        output "virtual ";
        output (if return_type = "Void" then "void" else return_type);
        output (" " ^ remap_name ^ "(");
        output (ctx_arg_list ctx tl "");
        output
          (") "
          ^ (if return_type = "void" then "{}" else "{ return 0; }")
          ^ "\n");
        if doDynamic then output ("\t\t::Dynamic " ^ remap_name ^ "_dyn();\n")
    | _ when has_decl -> output (remap_name ^ "_decl " ^ remap_name ^ ";\n")
    (* Variable access *)
    | _ -> (
        (* Variable access *)
        let tcpp = cpp_type_of field.cf_type in
        let tcppStr = tcpp_to_string tcpp in
        if (not is_static) && only_stack_access field.cf_type then
          abort
            ("Variables of type " ^ tcppStr ^ " may not be used as members")
            field.cf_pos;

        output (tcppStr ^ " " ^ remap_name ^ ";\n");
        (if (not is_static) && is_gc_element ctx tcpp then
           let getPtr =
             match tcpp with TCppString -> ".raw_ref()" | _ -> ".mPtr"
           in
           output
             ("\t\tinline " ^ tcppStr ^ " _hx_set_" ^ remap_name
            ^ "(::hx::StackContext *_hx_ctx," ^ tcppStr
            ^ " _hx_v) { HX_OBJ_WB(this,_hx_v" ^ getPtr ^ ") return "
            ^ remap_name ^ "=_hx_v; }\n"));

        (* Add a "dyn" function for variable to unify variable/function access *)
        match follow field.cf_type with
        | _ when nativeGen -> ()
        | TFun (_, _) ->
            output (if is_static then "\t\tstatic " else "\t\t");
            output
              ("Dynamic " ^ remap_name ^ "_dyn() { return " ^ remap_name
             ^ ";}\n")
        | _ -> (
            (match field.cf_kind with
            | Var { v_read = AccCall }
              when (not is_static)
                   && is_dynamic_accessor ("get_" ^ field.cf_name) "get" field
                        class_def ->
                output ("\t\tDynamic get_" ^ field.cf_name ^ ";\n")
            | _ -> ());
            match field.cf_kind with
            | Var { v_write = AccCall }
              when (not is_static)
                   && is_dynamic_accessor ("set_" ^ field.cf_name) "set" field
                        class_def ->
                output ("\t\tDynamic set_" ^ field.cf_name ^ ";\n")
            | _ -> ()))

let generate baseCtx class_def =
  let common_ctx = baseCtx.ctx_common in
  let class_path = class_def.cl_path in
  let nativeGen = Meta.has Meta.NativeGen class_def.cl_meta in
  let smart_class_name = snd class_path in
  let scriptable =
    Common.defined common_ctx Define.Scriptable && not class_def.cl_private
  in
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

  (*let cpp_file = new_cpp_file common_ctx.file class_path in*)
  let debug =
    if
      Meta.has Meta.NoDebug class_def.cl_meta
      || Common.defined baseCtx.ctx_common Define.NoDebug
    then 0
    else 1
  in

  let h_file = new_header_file common_ctx common_ctx.file class_path in
  let ctx = file_context baseCtx h_file debug true in
  let strq = strq ctx.ctx_common in

  let parent, super =
    match class_def.cl_super with
    | Some (klass, params) ->
        let name =
          tcpp_to_string_suffix "_obj" (cpp_instance_type klass params)
        in
        ( (if has_class_flag class_def CInterface && nativeGen then "virtual "
           else "")
          ^ name,
          name )
    | None when nativeGen && has_class_flag class_def CInterface ->
        ("virtual ::hx::NativeInterface", "::hx::NativeInterface")
    | None when has_class_flag class_def CInterface -> ("", "::hx::Object")
    | None when nativeGen -> ("", "")
    | None -> ("::hx::Object", "::hx::Object")
  in
  let output_h = h_file#write in
  let def_string = join_class_path class_path "_" in

  begin_header_file h_file#write_h def_string nativeGen;

  (* Include the real header file for the super class *)
  (match class_def.cl_super with
  | Some super ->
      let klass = fst super in
      let include_files = get_all_meta_string_path klass.cl_meta Meta.Include in
      if List.length include_files > 0 then
        List.iter
          (fun inc -> h_file#add_include (path_of_string inc))
          include_files
      else h_file#add_include klass.cl_path
  | _ -> ());

  (* And any interfaces ... *)
  List.iter
    (fun imp ->
      let interface = fst imp in
      let include_files =
        get_all_meta_string_path interface.cl_meta Meta.Include
      in
      if List.length include_files > 0 then
        List.iter
          (fun inc -> h_file#add_include (path_of_string inc))
          include_files
      else h_file#add_include interface.cl_path)
    (real_interfaces class_def.cl_implements);

  (* Only need to forward-declare classes that are mentioned in the header file
     (ie, not the implementation) *)
  let super_deps = create_super_dependencies common_ctx in
  let header_referenced, header_flags =
    CppReferences.find_referenced_types_flags ctx (TClassDecl class_def) "*"
      super_deps (Hashtbl.create 0) true false scriptable
  in
  List.iter2
    (fun r f -> gen_forward_decl h_file r f)
    header_referenced header_flags;
  output_h "\n";

  output_h (get_class_code class_def Meta.HeaderCode);
  let includes =
    get_all_meta_string_path class_def.cl_meta Meta.HeaderInclude
  in
  let printer inc = output_h ("#include \"" ^ inc ^ "\"\n") in
  List.iter printer includes;

  begin_namespace output_h class_path;
  output_h "\n\n";
  output_h (get_class_code class_def Meta.HeaderNamespaceCode);

  let extern_class = Common.defined common_ctx Define.DllExport in
  let attribs =
    "HXCPP_" ^ (if extern_class then "EXTERN_" else "") ^ "CLASS_ATTRIBUTES"
  in

  let dump_native_interfaces () =
    List.iter
      (fun (c, params) ->
        output_h (" , public virtual " ^ join_class_path c.cl_path "::"))
      (List.filter
         (fun (t, _) -> is_native_gen_class t)
         class_def.cl_implements)
  in

  if has_class_flag class_def CInterface && not nativeGen then (
    output_h ("class " ^ attribs ^ " " ^ class_name ^ " {\n");
    output_h "\tpublic:\n";
    output_h ("\t\ttypedef " ^ super ^ " super;\n"))
  else if super = "" then (
    output_h ("class " ^ attribs ^ " " ^ class_name);
    dump_native_interfaces ();
    output_h "\n{\n\tpublic:\n")
  else (
    output_h ("class " ^ attribs ^ " " ^ class_name ^ " : public " ^ parent);
    dump_native_interfaces ();
    output_h "\n{\n\tpublic:\n";
    if not nativeGen then (
      output_h ("\t\ttypedef " ^ super ^ " super;\n");
      output_h ("\t\ttypedef " ^ class_name ^ " OBJ_;\n")));

  let classId =
    try Hashtbl.find baseCtx.ctx_type_ids (class_text class_def.cl_path)
    with Not_found -> Int32.zero
  in
  let classIdTxt = Printf.sprintf "0x%08lx" classId in

  if (not (has_class_flag class_def CInterface)) && not nativeGen then (
    output_h ("\t\t" ^ class_name ^ "();\n");
    output_h "\n\tpublic:\n";
    output_h ("\t\tenum { _hx_ClassId = " ^ classIdTxt ^ " };\n\n");
    output_h ("\t\tvoid __construct(" ^ constructor_type_args ^ ");\n");
    output_h
      ("\t\tinline void *operator new(size_t inSize, bool inContainer="
     ^ isContainer ^ ",const char *inName=" ^ gcName ^ ")\n");
    output_h
      "\t\t\t{ return ::hx::Object::operator new(inSize,inContainer,inName); }\n";
    output_h "\t\tinline void *operator new(size_t inSize, int extra)\n";
    output_h
      ("\t\t\t{ return ::hx::Object::operator new(inSize+extra," ^ isContainer
     ^ "," ^ gcName ^ "); }\n");
    if has_class_flag class_def CAbstract then output_h "\n"
    else if
      can_inline_constructor baseCtx class_def super_deps
        (create_constructor_dependencies common_ctx)
    then (
      output_h "\n";
      CppGen.generate_constructor ctx
        (fun str -> output_h ("\t\t" ^ str))
        class_def true)
    else (
      output_h
        ("\t\tstatic " ^ ptr_name ^ " __new(" ^ constructor_type_args ^ ");\n");
      if can_quick_alloc then
        output_h
          ("\t\tstatic " ^ ptr_name ^ " __alloc(::hx::Ctx *_hx_ctx"
          ^ (if constructor_type_args = "" then ""
             else "," ^ constructor_type_args)
          ^ ");\n"));
    if not (has_class_flag class_def CAbstract) then (
      output_h "\t\tstatic void * _hx_vtable;\n";
      output_h "\t\tstatic Dynamic __CreateEmpty();\n";
      output_h "\t\tstatic Dynamic __Create(::hx::DynamicArray inArgs);\n");
    if List.length (CppGen.dynamic_functions class_def) > 0 then
      output_h
        ("\t\tstatic void __alloc_dynamic_functions(::hx::Ctx *_hx_alloc,"
       ^ class_name ^ " *_hx_obj);\n");
    if scriptable then
      output_h "\t\tstatic ::hx::ScriptFunction __script_construct;\n";
    output_h ("\t\t//~" ^ class_name ^ "();\n\n");
    output_h "\t\tHX_DO_RTTI_ALL;\n";
    if has_get_member_field class_def then
      output_h
        "\t\t::hx::Val __Field(const ::String &inString, ::hx::PropertyAccess \
         inCallProp);\n";
    if has_get_static_field class_def then
      output_h
        "\t\tstatic bool __GetStatic(const ::String &inString, Dynamic \
         &outValue, ::hx::PropertyAccess inCallProp);\n";
    if has_set_member_field class_def then
      output_h
        "\t\t::hx::Val __SetField(const ::String &inString,const ::hx::Val \
         &inValue, ::hx::PropertyAccess inCallProp);\n";
    if has_set_static_field class_def then
      output_h
        "\t\tstatic bool __SetStatic(const ::String &inString, Dynamic \
         &ioValue, ::hx::PropertyAccess inCallProp);\n";
    if has_get_fields class_def then
      output_h "\t\tvoid __GetFields(Array< ::String> &outFields);\n";

    if has_compare_field class_def then
      output_h
        ("\t\tint __Compare(const ::hx::Object *inRHS) const { "
       ^ "return const_cast<" ^ class_name
       ^ " *>(this)->__compare(Dynamic((::hx::Object *)inRHS)); }\n");

    output_h "\t\tstatic void __register();\n";
    let native_gen = Meta.has Meta.NativeGen class_def.cl_meta in
    let needs_gc_funcs = (not native_gen) && has_new_gc_references class_def in
    if needs_gc_funcs then (
      output_h "\t\tvoid __Mark(HX_MARK_PARAMS);\n";
      output_h "\t\tvoid __Visit(HX_VISIT_PARAMS);\n");

    let haxe_implementations, native_implementations =
      CppGen.implementations class_def
    in
    let implements_haxe = Hashtbl.length haxe_implementations > 0 in
    let implements_native = Hashtbl.length native_implementations > 0 in

    if implements_native then (
      let implemented_instance_fields =
        List.filter should_implement_field class_def.cl_ordered_fields
      in
      let neededInterfaceFunctions =
        match implements_native with
        | true ->
            CppGen.needed_interface_functions implemented_instance_fields
              native_implementations
        | false -> []
      in

      output_h "\n\t\tHX_NATIVE_IMPLEMENTATION\n";
      List.iter
        (fun field ->
          match (follow field.cf_type, field.cf_kind) with
          | _, Method MethDynamic -> ()
          | TFun (args, return_type), _ ->
              let retVal = type_to_string return_type in
              let ret = if retVal = "void" then "" else "return " in
              let name = keyword_remap field.cf_name in
              let argNames =
                List.map (fun (name, _, _) -> keyword_remap name) args
              in
              output_h
                ("\t\t" ^ retVal ^ " " ^ name ^ "( "
                ^ print_tfun_arg_list true args
                ^ ") {\n");
              output_h
                ("\t\t\t" ^ ret ^ "super::" ^ name ^ "( "
               ^ String.concat "," argNames ^ ");\n\t\t}\n")
          | _ -> ())
        neededInterfaceFunctions;
      output_h "\n");

    output_h "\t\tbool _hx_isInstanceOf(int inClassId);\n";
    if implements_haxe then (
      output_h "\t\tvoid *_hx_getInterface(int inHash);\n";
      (* generate header glue *)
      let alreadyGlued = Hashtbl.create 0 in
      Hashtbl.iter
        (fun interface_name src ->
          let rec check_interface interface =
            let check_field field =
              match (follow field.cf_type, field.cf_kind) with
              | _, Method MethDynamic -> ()
              | TFun (args, return_type), Method _ ->
                  let cast = cpp_tfun_signature false args return_type in
                  let class_implementation =
                    find_class_implementation class_def field.cf_name interface
                  in
                  let realName = cpp_member_name_of field in
                  let castKey = realName ^ "::" ^ cast in
                  let castKey =
                    if interface_name = "_hx_haxe_IMap" && realName = "set" then
                      castKey ^ "*"
                    else castKey
                  in
                  let implementationKey =
                    realName ^ "::" ^ class_implementation
                  in
                  if castKey <> implementationKey then
                    let glue =
                      Printf.sprintf "%s_%08lx" field.cf_name
                        (gen_hash32 0 cast)
                    in
                    if not (Hashtbl.mem alreadyGlued castKey) then (
                      Hashtbl.replace alreadyGlued castKey ();
                      let argList = print_tfun_arg_list true args in
                      let returnType = type_to_string return_type in
                      let headerCode =
                        "\t\t" ^ returnType ^ " " ^ glue ^ "(" ^ argList
                        ^ ");\n"
                      in
                      output_h headerCode;
                      output_h "\n")
              | _ -> ()
            in
            (match interface.cl_super with
            | Some (super, _) -> check_interface super
            | _ -> ());
            List.iter check_field interface.cl_ordered_fields
          in
          check_interface src)
        haxe_implementations);

    if has_init_field class_def then output_h "\t\tstatic void __init__();\n\n";
    output_h
      ("\t\t::String __ToString() const { return " ^ strq smart_class_name
     ^ "; }\n\n"))
  else if not nativeGen then output_h "\t\tHX_DO_INTERFACE_RTTI;\n\n"
  else (
    CppGen.generate_native_constructor ctx output_h class_def true;
    (* native interface *) ());

  if has_boot_field class_def then output_h "\t\tstatic void __boot();\n";

  (match class_def.cl_array_access with
  | Some t -> output_h ("\t\ttypedef " ^ type_string t ^ " __array_access;\n")
  | _ -> ());

  List.iter
    (gen_member_def ctx class_def true (has_class_flag class_def CInterface))
    (List.filter should_implement_field class_def.cl_ordered_statics);

  let not_toString (field, args, _) =
    field.cf_name <> "toString" || has_class_flag class_def CInterface
  in
  let functions = List.filter not_toString (all_virtual_functions class_def) in
  if has_class_flag class_def CInterface then
    List.iter
      (fun (field, _, _) -> gen_member_def ctx class_def false true field)
      functions
  else
    List.iter
      (gen_member_def ctx class_def false false)
      (List.filter should_implement_field class_def.cl_ordered_fields);

  (if has_class_flag class_def CInterface then
     match get_meta_string class_def.cl_meta Meta.ObjcProtocol with
     | Some protocol ->
         output_h
           ("\t\tstatic id<" ^ protocol
          ^ "> _hx_toProtocol(Dynamic inImplementation);\n")
     | None -> ());

  output_h (get_class_code class_def Meta.HeaderClassCode);
  output_h "};\n\n";

  end_namespace output_h class_path;

  end_header_file output_h def_string;

  h_file#close