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

let cpp_get_interface_slot ctx name =
  try Hashtbl.find !(ctx.ctx_interface_slot) name
  with Not_found ->
    let result = !(ctx.ctx_interface_slot_count) in
    Hashtbl.replace !(ctx.ctx_interface_slot) name result;
    ctx.ctx_interface_slot_count := !(ctx.ctx_interface_slot_count) + 1;
    result

let generate_protocol_delegate ctx class_def output =
  let protocol = get_meta_string class_def.cl_meta Meta.ObjcProtocol |> Option.default "" in
  let full_class_name = ("::" ^ join_class_path_remap class_def.cl_path "::") ^ "_obj" in
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

let generate_managed_interface base_ctx tcpp_interface =
  let common_ctx = base_ctx.ctx_common in
  let class_path = tcpp_interface.if_class.cl_path in
  let cpp_file = new_placed_cpp_file base_ctx.ctx_common class_path in
  let cpp_ctx = file_context base_ctx cpp_file tcpp_interface.if_debug_level false in
  let ctx = cpp_ctx in
  let output_cpp = cpp_file#write in
  let strq = strq ctx.ctx_common in
  let scriptable = Common.defined common_ctx Define.Scriptable && not tcpp_interface.if_class.cl_private in

  if tcpp_interface.if_debug_level > 1 then
    print_endline
      ("Found interface definition:" ^ join_class_path tcpp_interface.if_class.cl_path "::");

  cpp_file#write_h "#include <hxcpp.h>\n\n";

  let all_referenced =
    CppReferences.find_referenced_types ctx (TClassDecl tcpp_interface.if_class) ctx.ctx_super_deps
    ctx.ctx_constructor_deps false false scriptable
  in
  List.iter (add_include cpp_file) all_referenced;

  if scriptable then cpp_file#write_h "#include <hx/Scriptable.h>\n";

  cpp_file#write_h "\n";

  output_cpp (get_class_code tcpp_interface.if_class Meta.CppFileCode);
  let includes = get_all_meta_string_path tcpp_interface.if_class.cl_meta Meta.CppInclude in
  let printer inc = output_cpp ("#include \"" ^ inc ^ "\"\n") in
  List.iter printer includes;

  begin_namespace output_cpp class_path;
  output_cpp "\n";

  output_cpp (get_class_code tcpp_interface.if_class Meta.CppNamespaceCode);

  output_cpp "\n";

  (* cl_interface *)
  let implemented_instance_fields = List.filter should_implement_field tcpp_interface.if_class.cl_ordered_fields in
  let reflective_members = List.filter (reflective tcpp_interface.if_class) implemented_instance_fields in
  let sMemberFields =
    match reflective_members with
    | [] -> "0 /* sMemberFields */"
    | _ ->
      let memberFields = tcpp_interface.if_name ^ "_sMemberFields" in
      let dump_field_name field = output_cpp ("\t" ^ strq field.cf_name ^ ",\n") in
      output_cpp ("static ::String " ^ memberFields ^ "[] = {\n");
      List.iter dump_field_name reflective_members;
      output_cpp "\t::String(null()) };\n\n";
      memberFields
  in

  if scriptable then (
    let dump_script_field idx func =
      let args = print_tfun_arg_list true func.iff_args in
      let return_type = type_to_string func.iff_return in
      let ret = if return_type = "Void" || return_type = "void" then " " else "return " in

      output_cpp ("\t" ^ return_type ^ " " ^ func.iff_name ^ "( " ^ args ^ " ) {\n");
      output_cpp "\t\t::hx::CppiaCtx *__ctx = ::hx::CppiaCtx::getCurrent();\n";
      output_cpp "\t\t::hx::AutoStack __as(__ctx);\n";
      output_cpp "\t\t__ctx->pushObject(this);\n";
      List.iter
        (fun (name, opt, t) ->
          output_cpp
            ("\t\t__ctx->push" ^ CppCppia.script_type t opt ^ "("
            ^ keyword_remap name ^ ");\n"))
        func.iff_args;
      let interfaceSlot = string_of_int (-cpp_get_interface_slot ctx func.iff_name) in
      output_cpp
        ("\t\t" ^ ret ^ "__ctx->run"
        ^ CppCppia.script_type func.iff_return false
        ^ "(__GetScriptVTable()[" ^ interfaceSlot ^ "]);\n");
      output_cpp "\t}\n";
    in

    let script_name = tcpp_interface.if_name ^ "__scriptable" in

    output_cpp ("class " ^ script_name ^ " : public ::hx::Object {\n");
    output_cpp "public:\n";

    let list_iteri func in_list =
      let idx = ref 0 in
      List.iter
        (fun elem ->
          func !idx elem;
          idx := !idx + 1)
        in_list
    in

    list_iteri dump_script_field tcpp_interface.if_functions;
    output_cpp "};\n\n";

    let generate_script_function func =
      let scriptName = ("__s_" ^ func.iff_field.cf_name) in

      output_cpp ("\nstatic void CPPIA_CALL " ^ scriptName ^ "(::hx::CppiaCtx *ctx) {\n");
      let ret =
        match cpp_type_of func.iff_return with
        | TCppScalar "bool" -> "b"
        | _ -> CppCppia.script_signature func.iff_return false in
      if ret <> "v" then
        output_cpp ("ctx->return" ^ CppCppia.script_type func.iff_return false ^ "(");

      let signature =
        output_cpp (tcpp_interface.if_name ^ "::" ^ func.iff_name ^ "(ctx->getThis()" ^ if List.length func.iff_args > 0 then "," else "");

        let signature, _, _ =
          List.fold_left
            (fun (signature, sep, size) (_, opt, t) ->
              output_cpp (sep ^ "ctx->get" ^ CppCppia.script_type t opt ^ "(" ^ size ^ ")");
              ( signature ^ CppCppia.script_signature t opt, ",", size ^ "+sizeof(" ^ CppCppia.script_size_type t opt ^ ")" ))
            (ret, "", "sizeof(void*)") func.iff_args in
        output_cpp ")";
        signature
      in

      if ret <> "v" then output_cpp ")";
      output_cpp ";\n}\n";
      (signature, func)
    in

    match tcpp_interface.if_functions with
    | [] ->
      output_cpp "static ::hx::ScriptNamedFunction *__scriptableFunctions = 0;\n"
    | _ ->
      let sig_and_funcs = List.map generate_script_function tcpp_interface.if_functions in

      output_cpp "#ifndef HXCPP_CPPIA_SUPER_ARG\n";
      output_cpp "#define HXCPP_CPPIA_SUPER_ARG(x)\n";
      output_cpp "#endif\n";
      output_cpp "static ::hx::ScriptNamedFunction __scriptableFunctions[] = {\n";
      let dump_func (s, func) =
        Printf.sprintf
          "\t::hx::ScriptNamedFunction(\"%s\", __s_%s, \"%s\", false HXCPP_CPPIA_SUPER_ARG(0)),\n"
          func.iff_field.cf_name
          func.iff_field.cf_name
          s |> output_cpp;
      in
      List.iter dump_func sig_and_funcs;
      output_cpp "\t::hx::ScriptNamedFunction(0,0,0 HXCPP_CPPIA_SUPER_ARG(0) ) };\n";

    let mapper f = Printf.sprintf "\t%s&%s::%s" (cpp_tfun_signature true f.iff_args f.iff_return) script_name f.iff_name in
    let strings =
      tcpp_interface.if_functions
      |> List.map mapper
      |> String.concat ",\n" in

    Printf.sprintf "\n\n%s %s_scriptable = {\n%s\n};\n" tcpp_interface.if_name tcpp_interface.if_name strings |> output_cpp);

  let class_name_text = join_class_path class_path "." in

  output_cpp ("::hx::Class " ^ tcpp_interface.if_name ^ "::__mClass;\n\n");

  output_cpp ("void " ^ tcpp_interface.if_name ^ "::__register()\n{\n");

  output_cpp "\t::hx::Static(__mClass) = new ::hx::Class_obj();\n";
  output_cpp ("\t__mClass->mName = " ^ strq class_name_text ^ ";\n");
  output_cpp "\t__mClass->mSuper = &super::__SGetClass();\n";
  output_cpp ("\t__mClass->mMembers = ::hx::Class_obj::dupFunctions(" ^ sMemberFields ^ ");\n");
  output_cpp ("\t__mClass->mCanCast = ::hx::TIsInterface< (int)" ^ tcpp_interface.if_hash ^ " >;\n");
  output_cpp "\t::hx::_hx_RegisterClass(__mClass->mName, __mClass);\n";
  if scriptable then
    output_cpp ("  HX_SCRIPTABLE_REGISTER_INTERFACE(\"" ^ class_name_text ^ "\"," ^ tcpp_interface.if_name ^ ");\n");
  output_cpp "}\n\n";

  if has_boot_field tcpp_interface.if_class then (
    output_cpp ("void " ^ tcpp_interface.if_name ^ "::__boot()\n{\n");

    let dot_name = join_class_path tcpp_interface.if_class.cl_path "." in

    (match tcpp_interface.if_meta with
    | Some expr -> gen_cpp_init ctx dot_name "boot" "__mClass->__meta__ = " expr
    | None -> ());

    (match tcpp_interface.if_rtti with
    | Some expr -> gen_cpp_init ctx dot_name "boot" "__mClass->__rtti__ = " expr
    | None -> ());

    output_cpp "}\n\n");

  end_namespace output_cpp class_path;

  if Meta.has Meta.ObjcProtocol tcpp_interface.if_class.cl_meta then (
    let full_class_name = ("::" ^ join_class_path_remap class_path "::") ^ "_obj" in
    let protocol = get_meta_string tcpp_interface.if_class.cl_meta Meta.ObjcProtocol |> Option.default "" in
    generate_protocol_delegate ctx tcpp_interface.if_class output_cpp;
    output_cpp ("id<" ^ protocol ^ "> " ^ full_class_name ^ "::_hx_toProtocol(Dynamic inImplementation) {\n");
    output_cpp ("\treturn [ [_hx_" ^ protocol ^ "_delegate alloc] initWithImplementation:inImplementation.mPtr];\n");
    output_cpp "}\n\n");

  cpp_file#close