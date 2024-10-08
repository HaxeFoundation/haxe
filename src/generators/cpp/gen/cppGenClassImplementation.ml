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

let gen_field ctx class_def class_name is_static field =
  ctx.ctx_real_this_ptr <- not is_static;

  let output = ctx.ctx_output in
  let remap_name = keyword_remap field.cf_name in
  let decl = get_meta_string field.cf_meta Meta.Decl in
  let has_decl = match decl with Some _ -> true | None -> false in
  match field.cf_expr with
  (* Function field *)
  | Some { eexpr = TFunction function_def } ->
      let return_type_str = type_to_string function_def.tf_type in
      let nargs = string_of_int (List.length function_def.tf_args) in
      let return_type = cpp_type_of function_def.tf_type in
      let is_void = return_type = TCppVoid in
      let ret = if is_void then "(void)" else "return " in

      let needsWrapper t =
        match t with
        | TCppStar _ -> true
        | TCppInst (t, _) -> Meta.has Meta.StructAccess t.cl_meta
        | _ -> false
      in
      let orig_debug = ctx.ctx_debug_level in
      let no_debug = Meta.has Meta.NoDebug field.cf_meta in

      if not (is_dynamic_haxe_method field) then (
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
              let wrap =
                needsWrapper return_type || List.exists needsWrapper tcpp_args
              in
              if wrap then (
                let wrapName = "_hx_wrap" ^ class_name ^ "_" ^ remap_name in
                output ("static ::Dynamic " ^ wrapName ^ "( ");
                let sep = ref " " in
                if not is_static then (
                  output "::hx::Object *obj";
                  sep := ",");
                ExtList.List.iteri
                  (fun i _ ->
                    output (!sep ^ "const Dynamic &a" ^ string_of_int i);
                    sep := ",")
                  tcpp_args;
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

                if is_static then output (class_name ^ "::" ^ remap_name ^ "(")
                else
                  output
                    ("reinterpret_cast< " ^ class_name ^ " *>(obj)->"
                   ^ remap_name ^ "(");

                sep := "";
                ExtList.List.iteri
                  (fun i arg ->
                    output !sep;
                    sep := ",";
                    (match arg with
                    | TCppStar (t, const) ->
                        output
                          ("(cpp::"
                          ^ (if const then "Const" else "")
                          ^ "Pointer<" ^ tcpp_to_string t ^ " >) ")
                    | TCppInst (t, _) when Meta.has Meta.StructAccess t.cl_meta
                      ->
                        output ("(cpp::Struct< " ^ tcpp_to_string arg ^ " >) ")
                    | _ -> ());
                    output ("a" ^ string_of_int i))
                  tcpp_args;

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
              else (
                if is_static then output "STATIC_";
                output
                  ("HX_DEFINE_DYNAMIC_FUNC" ^ nargs ^ "(" ^ class_name ^ ","
                 ^ remap_name ^ "," ^ ret ^ ")\n\n")))
      else (
        ctx.ctx_real_this_ptr <- false;
        let func_name = "__default_" ^ remap_name in
        output ("HX_BEGIN_DEFAULT_FUNC(" ^ func_name ^ "," ^ class_name ^ ")\n");
        output return_type_str;
        output
          (" _hx_run(" ^ print_arg_list function_def.tf_args "__o_" ^ ")");
        gen_cpp_function_body ctx class_def is_static func_name function_def ""
          "" no_debug;

        output ("HX_END_LOCAL_FUNC" ^ nargs ^ "(" ^ ret ^ ")\n");
        output "HX_END_DEFAULT_FUNC\n\n";

        if is_static then
          output ("::Dynamic " ^ class_name ^ "::" ^ remap_name ^ ";\n\n"));
      ctx.ctx_debug_level <- orig_debug
  (* Data field *)
  | _ when has_decl ->
      if is_static then (
        output (class_name ^ "::" ^ remap_name ^ "_decl ");
        output (" " ^ class_name ^ "::" ^ remap_name ^ ";\n\n"))
  | _ ->
      if is_static && is_physical_field field then (
        gen_type ctx field.cf_type;
        output (" " ^ class_name ^ "::" ^ remap_name ^ ";\n\n"))
      else if has_class_field_flag field CfAbstract then
        let tl, tr =
          match follow field.cf_type with
          | TFun (tl, tr) -> (tl, tr)
          | _ -> die "" __LOC__
        in
        let nargs = string_of_int (List.length tl) in
        let return_type = cpp_type_of tr in
        let is_void = return_type = TCppVoid in
        let ret = if is_void then "(void)" else "return " in
        output
          ("HX_DEFINE_DYNAMIC_FUNC" ^ nargs ^ "(" ^ class_name ^ ","
         ^ remap_name ^ "," ^ ret ^ ")\n\n")

let gen_field_init ctx class_def field =
  let dot_name = join_class_path class_def.cl_path "." in
  let output = ctx.ctx_output in
  let remap_name = keyword_remap field.cf_name in

  match field.cf_expr with
  (* Function field *)
  | Some { eexpr = TFunction function_def } ->
      if is_dynamic_haxe_method field then
        let func_name = "__default_" ^ remap_name in
        output ("\t" ^ remap_name ^ " = new " ^ func_name ^ ";\n\n")
  (* Data field *)
  | Some expr ->
      let var_name =
        match remap_name with
        | "__meta__" -> "__mClass->__meta__"
        | "__rtti" -> "__mClass->__rtti__"
        | _ -> remap_name
      in

      gen_cpp_init ctx dot_name "boot" (var_name ^ " = ") expr
  | _ -> ()

let cpp_get_interface_slot ctx name =
  try Hashtbl.find !(ctx.ctx_interface_slot) name
  with Not_found ->
    let result = !(ctx.ctx_interface_slot_count) in
    Hashtbl.replace !(ctx.ctx_interface_slot) name result;
    ctx.ctx_interface_slot_count := !(ctx.ctx_interface_slot_count) + 1;
    result

let generate_protocol_delegate ctx class_def output =
  let protocol =
    get_meta_string class_def.cl_meta Meta.ObjcProtocol |> Option.default ""
  in
  let full_class_name =
    ("::" ^ join_class_path_remap class_def.cl_path "::") ^ "_obj"
  in
  let name = "_hx_" ^ protocol ^ "_delegate" in
  output ("@interface " ^ name ^ " : NSObject<" ^ protocol ^ "> {\n");
  output "\t::hx::Object *haxeObj;\n";
  output "}\n";
  output "@end\n\n";
  output ("@implementation " ^ name ^ "\n");
  output "- (id)initWithImplementation:( ::hx::Object *)inInplemnetation {\n";
  output "  if (self = [super init]) {\n";
  output "     self->haxeObj = inInplemnetation;\n";
  output "     GCAddRoot(&self->haxeObj);\n";
  output "  }\n";
  output "  return self;\n";
  output "}\n";
  output "- (void)dealloc {\n";
  output "   GCRemoveRoot(&self->haxeObj);\n";
  output "   #ifndef OBJC_ARC\n";
  output "   [super dealloc];\n";
  output "   #endif\n";
  output "}\n\n";

  let dump_delegate field =
    match field.cf_type with
    | TFun (args, ret) ->
        let retStr = type_to_string ret in
        let fieldName, argNames =
          match get_meta_string field.cf_meta Meta.ObjcProtocol with
          | Some nativeName ->
              let parts = ExtString.String.nsplit nativeName ":" in
              (List.hd parts, parts)
          | None -> (field.cf_name, List.map (fun (n, _, _) -> n) args)
        in
        output ("- (" ^ retStr ^ ") " ^ fieldName);

        let first = ref true in
        (try
           List.iter2
             (fun (name, _, argType) signature_name ->
               if !first then
                 output (" :(" ^ type_to_string argType ^ ")" ^ name)
               else
                 output
                   (" " ^ signature_name ^ ":(" ^ type_to_string argType ^ ")"
                  ^ name);
               first := false)
             args argNames
         with Invalid_argument _ ->
           abort
             (let argString =
                String.concat "," (List.map (fun (name, _, _) -> name) args)
              in
              "Invalid arg count in delegate in " ^ field.cf_name ^ " '"
              ^ field.cf_name ^ "," ^ argString ^ "' != '"
              ^ String.concat "," argNames ^ "'")
             field.cf_pos);
        output " {\n";
        output "\t::hx::NativeAttach _hx_attach;\n";
        output
          ((if retStr = "void" then "\t" else "\treturn ")
          ^ full_class_name ^ "::"
          ^ keyword_remap field.cf_name
          ^ "(haxeObj");
        List.iter (fun (name, _, _) -> output ("," ^ name)) args;
        output ");\n}\n\n"
    | _ -> ()
  in
  List.iter dump_delegate class_def.cl_ordered_fields;

  output "@end\n\n"

let generate baseCtx class_def =
  let common_ctx = baseCtx.ctx_common in
  let class_path = class_def.cl_path in
  let debug = baseCtx.ctx_debug_level in
  let cpp_file = new_placed_cpp_file baseCtx.ctx_common class_path in
  let cpp_ctx = file_context baseCtx cpp_file debug false in
  let ctx = cpp_ctx in
  let output_cpp = cpp_file#write in
  let strq = strq ctx.ctx_common in
  let scriptable =
    Common.defined common_ctx Define.Scriptable && not class_def.cl_private
  in

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

  let constructor_deps = create_constructor_dependencies common_ctx in
  let super_deps = create_super_dependencies common_ctx in
  let all_referenced =
    CppReferences.find_referenced_types ctx (TClassDecl class_def) super_deps
      constructor_deps false false scriptable
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

  let nativeGen = Meta.has Meta.NativeGen class_def.cl_meta in
  let class_name = class_name class_def in
  let cargs = constructor_arg_var_list class_def in
  let constructor_type_var_list = List.map snd cargs in
  let constructor_var_list = List.map snd constructor_type_var_list in
  let constructor_type_args =
    String.concat ","
      (List.map (fun (t, a) -> t ^ " " ^ a) constructor_type_var_list)
  in
  let haxe_implementations, native_implementations =
    implementations class_def
  in

  if (not (has_class_flag class_def CInterface)) && not nativeGen then (
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
        (* Convert an array to a comma separated list of values *)
        let i = ref (0 - 1) in
        String.concat ","
          (List.map
             (fun _ ->
               incr i;
               "inArgs[" ^ string_of_int !i ^ "]")
             inList)
      in

      output_cpp
        ("Dynamic " ^ class_name ^ "::__CreateEmpty() { return new "
       ^ class_name ^ "; }\n\n");
      output_cpp ("void *" ^ class_name ^ "::_hx_vtable = 0;\n\n");

      output_cpp
        ("Dynamic " ^ class_name ^ "::__Create(::hx::DynamicArray inArgs)\n");
      output_cpp
        ("{\n\t" ^ ptr_name ^ " _hx_result = new " ^ class_name ^ "();\n");
      output_cpp
        ("\t_hx_result->__construct("
        ^ array_arg_list constructor_var_list
        ^ ");\n");
      output_cpp "\treturn _hx_result;\n}\n\n");
    let rec addParent cls others =
      match cls.cl_super with
      | Some (super, _) -> (
          try
            let parentId =
              Hashtbl.find ctx.ctx_type_ids (class_text super.cl_path)
            in
            addParent super (parentId :: others)
          with Not_found -> others)
      | _ -> others
    in
    let classId =
      try Hashtbl.find baseCtx.ctx_type_ids (class_text class_def.cl_path)
      with Not_found -> Int32.zero
    in
    let implemented_classes = addParent class_def [ classId; Int32.of_int 1 ] in
    let implemented_classes = List.sort compare implemented_classes in

    output_cpp ("bool " ^ class_name ^ "::_hx_isInstanceOf(int inClassId) {\n");
    let txt cId = Printf.sprintf "0x%08lx" cId in
    let rec dump_classes indent classes =
      match classes with
      | [] -> ()
      | [ c ] -> output_cpp (indent ^ "return inClassId==(int)" ^ txt c ^ ";\n")
      | [ c; c1 ] ->
          output_cpp
            (indent ^ "return inClassId==(int)" ^ txt c ^ " || inClassId==(int)"
           ^ txt c1 ^ ";\n")
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

    let implements_haxe_keys = hash_keys haxe_implementations in
    let implements_haxe = Hashtbl.length haxe_implementations > 0 in

    if implements_haxe then (
      let alreadyGlued = Hashtbl.create 0 in
      let cname = "_hx_" ^ join_class_path class_def.cl_path "_" in
      let implname = cpp_class_name class_def in
      let cpp_glue = ref [] in
      List.iter
        (fun interface_name ->
          try
            let interface = Hashtbl.find haxe_implementations interface_name in
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
            output_cpp "};\n\n"
          with Not_found -> ())
        implements_haxe_keys;

      output_cpp (String.concat "\n" !cpp_glue);

      output_cpp ("void *" ^ class_name ^ "::_hx_getInterface(int inHash) {\n");
      output_cpp "\tswitch(inHash) {\n";
      List.iter
        (fun interface_name ->
          try
            let interface = Hashtbl.find haxe_implementations interface_name in
            output_cpp
              ("\t\tcase (int)" ^ cpp_class_hash interface ^ ": return &"
             ^ cname ^ "_" ^ interface_name ^ ";\n")
          with Not_found -> ())
        implements_haxe_keys;

      output_cpp "\t}\n";

      if class_super_name = "" then (
        output_cpp "\t#ifdef HXCPP_SCRIPTABLE\n";
        output_cpp "\treturn super::_hx_getInterface(inHash);\n";
        output_cpp "\t#else\n";
        output_cpp "\treturn 0;\n";
        output_cpp "\t#endif\n")
      else output_cpp "\treturn super::_hx_getInterface(inHash);\n";
      output_cpp "}\n\n"));

  (match TClass.get_cl_init class_def with
  | Some expression ->
      let ctx = file_context baseCtx cpp_file debug false in
      output_cpp ("void " ^ class_name ^ "::__init__()");
      gen_cpp_init ctx (cpp_class_name class_def) "__init__" ""
        (mk_block expression);
      output_cpp "\n\n"
  | _ -> ());

  let dump_field_name field = output_cpp ("\t" ^ strq field.cf_name ^ ",\n") in
  let statics_except_meta = statics_except_meta class_def in
  let implemented_fields =
    List.filter should_implement_field statics_except_meta
  in

  List.iter
    (gen_field ctx class_def class_name false)
    class_def.cl_ordered_fields;
  List.iter (gen_field ctx class_def class_name true) statics_except_meta;
  output_cpp "\n";

  let dynamic_functions = dynamic_functions class_def in
  if List.length dynamic_functions > 0 then (
    output_cpp
      ("void " ^ class_name ^ "::__alloc_dynamic_functions(::hx::Ctx *_hx_ctx,"
     ^ class_name ^ " *_hx_obj) {\n");
    List.iter
      (fun name ->
        output_cpp
          ("\tif (!_hx_obj->" ^ name ^ ".mPtr) _hx_obj->" ^ name
         ^ " = new __default_" ^ name ^ "(_hx_obj);\n"))
      dynamic_functions;
    (match class_def.cl_super with
    | Some super ->
        let rec find_super class_def =
          if has_dynamic_member_functions class_def then
            let super_name =
              join_class_path_remap class_def.cl_path "::" ^ "_obj"
            in
            output_cpp
              ("\t" ^ super_name
             ^ "::__alloc_dynamic_functions(_hx_ctx,_hx_obj);\n")
          else
            match class_def.cl_super with
            | Some super -> find_super (fst super)
            | _ -> ()
        in
        find_super (fst super)
    | _ -> ());
    output_cpp "}\n");

  let inline_constructor =
    can_inline_constructor baseCtx class_def super_deps
      (create_constructor_dependencies common_ctx)
  in
  if
    (not (has_class_flag class_def CInterface))
    && (not nativeGen) && (not inline_constructor)
    && not (has_class_flag class_def CAbstract)
  then generate_constructor ctx output_cpp class_def false
  else if nativeGen then
    generate_native_constructor ctx output_cpp class_def false;

  let reflect_member_fields =
    List.filter (reflective class_def) class_def.cl_ordered_fields
  in
  let reflect_static_fields =
    List.filter (reflective class_def) statics_except_meta
  in

  (* Initialise non-static variables *)
  if (not (has_class_flag class_def CInterface)) && not nativeGen then (
    output_cpp (class_name ^ "::" ^ class_name ^ "()\n{\n");
    List.iter
      (fun name ->
        output_cpp ("\t" ^ name ^ " = new __default_" ^ name ^ "(this);\n"))
      dynamic_functions;
    output_cpp "}\n\n";

    let dump_field_iterator macro field =
      if is_data_member field then (
        let remap_name = keyword_remap field.cf_name in
        output_cpp
          ("\t" ^ macro ^ "(" ^ remap_name ^ ",\"" ^ field.cf_name ^ "\");\n");

        (match field.cf_kind with
        | Var { v_read = AccCall }
          when is_dynamic_accessor ("get_" ^ field.cf_name) "get" field
                 class_def ->
            let name = "get_" ^ field.cf_name in
            output_cpp ("\t" ^ macro ^ "(" ^ name ^ "," ^ "\"" ^ name ^ "\");\n")
        | _ -> ());
        match field.cf_kind with
        | Var { v_write = AccCall }
          when is_dynamic_accessor ("set_" ^ field.cf_name) "set" field
                 class_def ->
            let name = "set_" ^ field.cf_name in
            output_cpp ("\t" ^ macro ^ "(" ^ name ^ "," ^ "\"" ^ name ^ "\");\n")
        | _ -> ())
    in

    let implemented_instance_fields =
      List.filter should_implement_field class_def.cl_ordered_fields
    in

    let override_iteration =
      (not nativeGen) && has_new_gc_references class_def
    in
    if override_iteration then (
      let super_needs_iteration = find_next_super_iteration class_def in
      let smart_class_name = snd class_path in
      (* MARK function - explicitly mark all child pointers *)
      output_cpp ("void " ^ class_name ^ "::__Mark(HX_MARK_PARAMS)\n{\n");
      output_cpp ("\tHX_MARK_BEGIN_CLASS(" ^ smart_class_name ^ ");\n");
      List.iter
        (dump_field_iterator "HX_MARK_MEMBER_NAME")
        implemented_instance_fields;
      (match super_needs_iteration with
      | "" -> ()
      | super -> output_cpp ("\t" ^ super ^ "::__Mark(HX_MARK_ARG);\n"));
      output_cpp "\tHX_MARK_END_CLASS();\n";
      output_cpp "}\n\n";

      (* Visit function - explicitly visit all child pointers *)
      output_cpp ("void " ^ class_name ^ "::__Visit(HX_VISIT_PARAMS)\n{\n");
      List.iter
        (dump_field_iterator "HX_VISIT_MEMBER_NAME")
        implemented_instance_fields;
      (match super_needs_iteration with
      | "" -> ()
      | super -> output_cpp ("\t" ^ super ^ "::__Visit(HX_VISIT_ARG);\n"));
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
      output_cpp
        ("\t{" ^ storage field ^ ",(int)offsetof(" ^ class_name ^ ","
        ^ keyword_remap field.cf_name
        ^ ")," ^ strq field.cf_name ^ "},\n")
    in
    let dump_static_storage field =
      output_cpp
        ("\t{" ^ storage field ^ ",(void *) &" ^ class_name ^ "::"
        ^ keyword_remap field.cf_name
        ^ "," ^ strq field.cf_name ^ "},\n")
    in

    output_cpp "#ifdef HXCPP_SCRIPTABLE\n";

    let stored_fields =
      List.filter is_data_member implemented_instance_fields
    in
    if List.length stored_fields > 0 then (
      output_cpp
        ("static ::hx::StorageInfo " ^ class_name
       ^ "_sMemberStorageInfo[] = {\n");
      List.iter dump_member_storage stored_fields;
      output_cpp "\t{ ::hx::fsUnknown, 0, null()}\n};\n")
    else
      output_cpp
        ("static ::hx::StorageInfo *" ^ class_name
       ^ "_sMemberStorageInfo = 0;\n");

    let stored_statics = List.filter is_data_member implemented_fields in
    if List.length stored_statics > 0 then (
      output_cpp
        ("static ::hx::StaticInfo " ^ class_name ^ "_sStaticStorageInfo[] = {\n");
      List.iter dump_static_storage stored_statics;
      output_cpp "\t{ ::hx::fsUnknown, 0, null()}\n};\n")
    else
      output_cpp
        ("static ::hx::StaticInfo *" ^ class_name ^ "_sStaticStorageInfo = 0;\n");

    output_cpp "#endif\n\n");

  (* cl_interface *)
  let implemented_instance_fields =
    List.filter should_implement_field class_def.cl_ordered_fields
  in
  let reflective_members =
    List.filter (reflective class_def) implemented_instance_fields
  in
  let sMemberFields =
    match reflective_members with
    | [] -> "0 /* sMemberFields */"
    | _ ->
        let memberFields = class_name ^ "_sMemberFields" in
        output_cpp ("static ::String " ^ memberFields ^ "[] = {\n");
        List.iter dump_field_name reflective_members;
        output_cpp "\t::String(null()) };\n\n";
        memberFields
  in

  let hasMarkFunc =
    (not nativeGen) && List.exists is_data_member implemented_fields
  in

  if hasMarkFunc then (
    (* Mark static variables as used *)
    output_cpp
      ("static void " ^ class_name ^ "_sMarkStatics(HX_MARK_PARAMS) {\n");
    List.iter
      (fun field ->
        if is_data_member field then
          output_cpp
            ("\tHX_MARK_MEMBER_NAME(" ^ class_name ^ "::"
            ^ keyword_remap field.cf_name
            ^ ",\"" ^ field.cf_name ^ "\");\n"))
      implemented_fields;
    output_cpp "};\n\n";

    (* Visit static variables *)
    output_cpp "#ifdef HXCPP_VISIT_ALLOCS\n";
    output_cpp
      ("static void " ^ class_name ^ "_sVisitStatics(HX_VISIT_PARAMS) {\n");
    List.iter
      (fun field ->
        if is_data_member field then
          output_cpp
            ("\tHX_VISIT_MEMBER_NAME(" ^ class_name ^ "::"
            ^ keyword_remap field.cf_name
            ^ ",\"" ^ field.cf_name ^ "\");\n"))
      implemented_fields;
    output_cpp "};\n\n";
    output_cpp "#endif\n\n");

  let generate_script_function isStatic field scriptName callName =
    match follow field.cf_type with
    | TFun (args, return_type) when not (is_data_member field) ->
        let isTemplated =
          (not isStatic) && not (has_class_flag class_def CInterface)
        in
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
          if has_class_flag class_def CInterface then
            output_cpp
              (class_name ^ "::" ^ callName ^ "(ctx->getThis()"
              ^ if List.length args > 0 then "," else "")
          else if isStatic then output_cpp (class_name ^ "::" ^ callName ^ "(")
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

  let newInteface = has_class_flag class_def CInterface in

  if scriptable && not nativeGen then (
    let delegate = "this->" in
    let dump_script_field idx (field, f_args, return_t) =
      let args = print_tfun_arg_list true f_args in
      let names = List.map (fun (n, _, _) -> keyword_remap n) f_args in
      let return_type = type_to_string return_t in
      let ret =
        if return_type = "Void" || return_type = "void" then " " else "return "
      in
      let name = keyword_remap field.cf_name in
      let vtable = "__scriptVTable[" ^ string_of_int (idx + 1) ^ "] " in
      let args_varray =
        List.fold_left
          (fun l n -> l ^ ".Add(" ^ n ^ ")")
          "Array<Dynamic>()" names
      in

      output_cpp ("\t" ^ return_type ^ " " ^ name ^ "( " ^ args ^ " ) {\n");
      if newInteface then (
        output_cpp "\t\t::hx::CppiaCtx *__ctx = ::hx::CppiaCtx::getCurrent();\n";
        output_cpp "\t\t::hx::AutoStack __as(__ctx);\n";
        output_cpp "\t\t__ctx->pushObject(this);\n";
        List.iter
          (fun (name, opt, t) ->
            output_cpp
              ("\t\t__ctx->push" ^ CppCppia.script_type t opt ^ "("
             ^ keyword_remap name ^ ");\n"))
          f_args;
        let interfaceSlot = string_of_int (-cpp_get_interface_slot ctx name) in
        output_cpp
          ("\t\t" ^ ret ^ "__ctx->run"
          ^ CppCppia.script_type return_t false
          ^ "(__GetScriptVTable()[" ^ interfaceSlot ^ "]);\n");
        output_cpp "\t}\n")
      else (
        output_cpp ("\tif (" ^ vtable ^ ") {\n");
        output_cpp "\t\t::hx::CppiaCtx *__ctx = ::hx::CppiaCtx::getCurrent();\n";
        output_cpp "\t\t::hx::AutoStack __as(__ctx);\n";
        output_cpp
          ("\t\t__ctx->pushObject("
          ^ (if has_class_flag class_def CInterface then "mDelegate.mPtr"
             else "this")
          ^ ");\n");
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

        if has_class_flag class_def CInterface then (
          output_cpp
            (" " ^ delegate ^ "__Field(HX_CSTRING(\"" ^ field.cf_name
           ^ "\"), ::hx::paccNever)");
          if List.length names <= 5 then
            output_cpp ("->__run(" ^ String.concat "," names ^ ");")
          else output_cpp ("->__Run(" ^ args_varray ^ ");"))
        else
          output_cpp
            (class_name ^ "::" ^ name ^ "(" ^ String.concat "," names ^ ");");
        if return_type <> "void" then output_cpp "return null();";
        output_cpp "}\n";
        let dynamic_interface_closures =
          Common.defined baseCtx.ctx_common Define.DynamicInterfaceClosures
        in
        if has_class_flag class_def CInterface && not dynamic_interface_closures
        then
          output_cpp
            ("\tDynamic " ^ name
           ^ "_dyn() { return mDelegate->__Field(HX_CSTRING(\"" ^ field.cf_name
           ^ "\"), ::hx::paccNever); }\n\n"))
    in

    let new_sctipt_functions =
      if newInteface then all_virtual_functions class_def
      else List.rev (current_virtual_functions_rev class_def [])
    in
    let sctipt_name = class_name ^ "__scriptable" in

    if newInteface then (
      output_cpp ("class " ^ sctipt_name ^ " : public ::hx::Object {\n");
      output_cpp "public:\n")
    else (
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
      if has_class_flag class_def CInterface then
        output_cpp "   HX_DEFINE_SCRIPTABLE_INTERFACE\n"
      else (
        output_cpp
          ("   HX_DEFINE_SCRIPTABLE(HX_ARR_LIST"
          ^ string_of_int (List.length constructor_var_list)
          ^ ")\n");
        output_cpp "\tHX_DEFINE_SCRIPTABLE_DYNAMIC;\n"));

    let list_iteri func in_list =
      let idx = ref 0 in
      List.iter
        (fun elem ->
          func !idx elem;
          idx := !idx + 1)
        in_list
    in

    let not_toString (field, args, _) =
      field.cf_name <> "toString" || has_class_flag class_def CInterface
    in
    let functions =
      List.filter not_toString (all_virtual_functions class_def)
    in
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
          if isStaticFlag = "true" || has_class_flag class_def CInterface then
            "0"
          else "__s_" ^ f.cf_name ^ "<true>"
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
        "static ::hx::ScriptNamedFunction *__scriptableFunctions = 0;\n";

    if newInteface then (
      output_cpp ("\n\n" ^ class_name ^ " " ^ class_name ^ "_scriptable = {\n");
      List.iter
        (fun (f, args, return_type) ->
          let cast = cpp_tfun_signature true args return_type in
          output_cpp
            ("\t" ^ cast ^ "&" ^ sctipt_name ^ "::" ^ keyword_remap f.cf_name
           ^ ",\n"))
        new_sctipt_functions;
      output_cpp "};\n"));

  let class_name_text = join_class_path class_path "." in

  (* Initialise static in boot function ... *)
  if (not (has_class_flag class_def CInterface)) && not nativeGen then (
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

    let reflective_statics =
      List.filter (reflective class_def) implemented_fields
    in
    let sStaticFields =
      if List.length reflective_statics > 0 then (
        output_cpp ("static ::String " ^ class_name ^ "_sStaticFields[] = {\n");
        List.iter dump_field_name reflective_statics;
        output_cpp "\t::String(null())\n};\n\n";
        class_name ^ "_sStaticFields")
      else "0 /* sStaticFields */"
    in

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
    if hasMarkFunc then
      output_cpp ("\t__mClass->mMarkFunc = " ^ class_name ^ "_sMarkStatics;\n");
    output_cpp
      ("\t__mClass->mStatics = ::hx::Class_obj::dupFunctions(" ^ sStaticFields
     ^ ");\n");
    output_cpp
      ("\t__mClass->mMembers = ::hx::Class_obj::dupFunctions(" ^ sMemberFields
     ^ ");\n");
    output_cpp ("\t__mClass->mCanCast = ::hx::TCanCast< " ^ class_name ^ " >;\n");
    if hasMarkFunc then
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
    Hashtbl.iter
      (fun _ intf_def ->
        output_cpp
          ("\tHX_REGISTER_VTABLE_OFFSET( " ^ class_name ^ ","
          ^ join_class_path_remap intf_def.cl_path "::"
          ^ ");\n"))
      native_implementations;
    output_cpp "}\n\n")
  else if not nativeGen then (
    output_cpp ("::hx::Class " ^ class_name ^ "::__mClass;\n\n");

    output_cpp ("void " ^ class_name ^ "::__register()\n{\n");

    output_cpp "\t::hx::Static(__mClass) = new ::hx::Class_obj();\n";
    output_cpp ("\t__mClass->mName = " ^ strq class_name_text ^ ";\n");
    output_cpp "\t__mClass->mSuper = &super::__SGetClass();\n";
    if hasMarkFunc then
      output_cpp ("\t__mClass->mMarkFunc = " ^ class_name ^ "_sMarkStatics;\n");
    output_cpp
      ("\t__mClass->mMembers = ::hx::Class_obj::dupFunctions(" ^ sMemberFields
     ^ ");\n");
    output_cpp
      ("\t__mClass->mCanCast = ::hx::TIsInterface< (int)"
     ^ cpp_class_hash class_def ^ " >;\n");
    if hasMarkFunc then
      output_cpp
        ("#ifdef HXCPP_VISIT_ALLOCS\n\t__mClass->mVisitFunc = " ^ class_name
       ^ "_sVisitStatics;\n#endif\n");
    output_cpp "\t::hx::_hx_RegisterClass(__mClass->mName, __mClass);\n";
    if scriptable then
      output_cpp
        ("  HX_SCRIPTABLE_REGISTER_INTERFACE(\"" ^ class_name_text ^ "\","
       ^ class_name ^ ");\n");
    output_cpp "}\n\n");

  if has_boot_field class_def then (
    output_cpp ("void " ^ class_name ^ "::__boot()\n{\n");

    List.iter
      (gen_field_init ctx class_def)
      (List.filter should_implement_field class_def.cl_ordered_statics);

    output_cpp "}\n\n");

  end_namespace output_cpp class_path;

  if
    has_class_flag class_def CInterface
    && Meta.has Meta.ObjcProtocol class_def.cl_meta
  then (
    let full_class_name =
      ("::" ^ join_class_path_remap class_path "::") ^ "_obj"
    in
    let protocol =
      get_meta_string class_def.cl_meta Meta.ObjcProtocol |> Option.default ""
    in
    generate_protocol_delegate ctx class_def output_cpp;
    output_cpp
      ("id<" ^ protocol ^ "> " ^ full_class_name
     ^ "::_hx_toProtocol(Dynamic inImplementation) {\n");
    output_cpp
      ("\treturn [ [_hx_" ^ protocol
     ^ "_delegate alloc] initWithImplementation:inImplementation.mPtr];\n");
    output_cpp "}\n\n");

  cpp_file#close