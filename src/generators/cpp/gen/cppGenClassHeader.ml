open Type
open Error
open Globals
open CppStrings
open CppTypeUtils
open CppAst
open CppAstTools
open CppSourceWriter
open CppContext
open CppGen

let gen_member_variable ctx class_def is_static (var:tcpp_class_variable) =
  let tcpp     = cpp_type_of var.tcv_type in
  let tcpp_str = tcpp_to_string tcpp in

  if not is_static && var.tcv_is_stackonly then
    abort (Printf.sprintf "%s is marked as stack only and therefor cannot be used as the type for a non static variable" tcpp_str) var.tcv_field.cf_pos;

  let output = ctx.ctx_output in
  let suffix = if is_static then "\t\tstatic " else "\t\t" in

  Printf.sprintf "%s%s %s;\n" suffix tcpp_str var.tcv_name |> output;

  if not is_static && var.tcv_is_gc_element then (
    let get_ptr = match tcpp with TCppString -> ".raw_ref()" | _ -> ".mPtr" in
    Printf.sprintf
      "\t\tinline %s _hx_set_%s(::hx::StackContext* _hx_ctx, %s _hx_v) { HX_OBJ_WB(this, _hx_v%s) return %s = _hx_v; }\n"
      tcpp_str var.tcv_name tcpp_str get_ptr var.tcv_name |> output;)

let gen_dynamic_function ctx class_def is_static func =
  let output = ctx.ctx_output in
  let prefix = if is_static then "\t\tstatic " else "\t\t" in

  Printf.sprintf "%sinline ::Dynamic& %s_dyn() { return %s; }\n" prefix func.tcf_name func.tcf_name |> output

let gen_member_function ctx class_def is_static func =
  let output = ctx.ctx_output in

  let fold_static acc = if is_static then "static" :: acc else acc in
  let fold_virtual acc =
    if not is_static && func.tcf_is_virtual then (
      if func.tcf_is_external && not func.tcf_is_scriptable then
        let key = Printf.sprintf "%s.%s" (join_class_path class_def.cl_path ".") func.tcf_field.cf_name in
        match StringMap.find_opt key ctx.ctx_class_member_types with
        | Some v -> v :: acc
        | None -> acc
      else
        "virtual" :: acc)
    else
      acc
  in

  let attributes = []
    |> fold_static
    |> fold_virtual
    |> String.concat " "
  in

  let return_type     = type_to_string func.tcf_func.tf_type in
  let return_type_str = if return_type = "Void" then "void" else return_type in
  Printf.sprintf "\t\t%s %s %s(%s);\n" attributes return_type_str func.tcf_name (print_arg_list func.tcf_func.tf_args "") |> output;

  if (not func.tcf_is_virtual || not func.tcf_is_overriding) && func.tcf_is_reflective then
    Printf.sprintf "\t\t%s::Dynamic %s_dyn();\n" (if is_static then "static " else "") func.tcf_name |> output;

  output "\n"

let gen_class_header ctx tcpp_class h_file scriptable parents =
  let class_path = tcpp_class.tcl_class.cl_path in
  let def_string = join_class_path class_path "_" in

  begin_header_file h_file#write_h def_string false;

  (* Include the real header file for the super class *)
  (match tcpp_class.tcl_class.cl_super with
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
    (real_interfaces tcpp_class.tcl_class.cl_implements);

  (* Only need to forward-declare classes that are mentioned in the header file
     (ie, not the implementation) *)
  let output_h   = h_file#write in
  let class_path = tcpp_class.tcl_class.cl_path in
  let header_referenced, header_flags =
    CppReferences.find_referenced_types_flags ctx (TClassDecl tcpp_class.tcl_class) None
    ctx.ctx_super_deps PathMap.empty true false scriptable
  in
  List.iter2
    (fun r f -> gen_forward_decl h_file r f)
    header_referenced header_flags;
  output_h "\n";

  output_h (get_class_code tcpp_class.tcl_class Meta.HeaderCode);
  let includes =
    get_all_meta_string_path tcpp_class.tcl_class.cl_meta Meta.HeaderInclude
  in
  let printer inc = output_h ("#include \"" ^ inc ^ "\"\n") in
  List.iter printer includes;

  begin_namespace output_h class_path;
  output_h "\n\n";
  output_h (get_class_code tcpp_class.tcl_class Meta.HeaderNamespaceCode);

  let extern_class = Gctx.defined ctx.ctx_common Define.DllExport in
  let attribs =
    "HXCPP_" ^ (if extern_class then "EXTERN_" else "") ^ "CLASS_ATTRIBUTES"
  in

  let folder acc (cls, _) =
    if is_native_class cls then
      (Printf.sprintf "public virtual %s" (join_class_path cls.cl_path "::")) :: acc
    else
      acc
    in
  let all_parents =
    tcpp_class.tcl_class.cl_implements
    |> List.fold_left folder parents
    |> List.rev in
  let parent_string =
    match all_parents with
    | [] -> ""
    | xs ->  " : " ^ String.concat ", " xs in

  Printf.sprintf "class %s %s%s\n{\n\tpublic:\n" attribs tcpp_class.tcl_name parent_string |> output_h

let generate_native_header base_ctx tcpp_class =
  let common_ctx = base_ctx.ctx_common in
  let class_def  = tcpp_class.tcl_class in
  let class_path = class_def.cl_path in
  let scriptable = has_tcpp_class_flag tcpp_class Scriptable in

  let h_file = new_header_file common_ctx common_ctx.file class_path in
  let ctx = file_context base_ctx h_file tcpp_class.tcl_debug_level true in

  let parent, super =
    match class_def.cl_super with
    | Some (klass, params) ->
        let name =
          tcpp_to_string_suffix "_obj" (cpp_instance_type klass params)
        in
        ( name, name )
    | None -> ("", "")
  in
  let output_h = h_file#write in
  let def_string = join_class_path class_path "_" in

  gen_class_header ctx tcpp_class h_file scriptable (if super = "" then [] else [ (Printf.sprintf "public %s" parent) ]);
      
  CppGen.generate_native_constructor ctx output_h class_def true;

  if has_tcpp_class_flag tcpp_class Boot then output_h "\t\tstatic void __boot();\n";

  tcpp_class.tcl_static_variables
  |> List.iter (gen_member_variable ctx class_def true);

  tcpp_class.tcl_static_functions
  |> List.iter (gen_member_function ctx class_def true);

  tcpp_class.tcl_static_dynamic_functions
  |> List.iter (gen_dynamic_function ctx class_def true);

  tcpp_class.tcl_variables
  |> List.iter (gen_member_variable ctx class_def false);

  tcpp_class.tcl_functions
  |> List.iter (gen_member_function ctx class_def false);

  tcpp_class.tcl_dynamic_functions
  |> List.iter (gen_dynamic_function ctx class_def false);

  output_h (get_class_code class_def Meta.HeaderClassCode);
  output_h "};\n\n";

  end_namespace output_h class_path;

  end_header_file output_h def_string;

  h_file#close

let generate_managed_header base_ctx tcpp_class =
  let common_ctx = base_ctx.ctx_common in
  let class_def = tcpp_class.tcl_class in
  let class_path = class_def.cl_path in
  let smart_class_name = snd class_path in
  let scriptable = has_tcpp_class_flag tcpp_class Scriptable in
  let class_name = tcpp_class.tcl_name in
  let ptr_name = class_pointer class_def in
  let can_quick_alloc = has_tcpp_class_flag tcpp_class QuickAlloc in
  let gcName = gen_gc_name class_def.cl_path in

  let constructor_type_args =
    tcpp_class.tcl_class
      |> constructor_arg_var_list
      |> List.map (fun (t, a) -> Printf.sprintf "%s %s" t a)
      |> String.concat "," in

  let h_file = new_header_file common_ctx common_ctx.file class_path in
  let ctx = file_context base_ctx h_file tcpp_class.tcl_debug_level true in
  let strq = strq ctx.ctx_common in

  let parent, super =
    match tcpp_class.tcl_super with
    | Some super ->
        let name = tcpp_to_string_suffix "_obj" (cpp_instance_type super.tcl_class super.tcl_params) in
        ( name, name )
    | None -> ("::hx::Object", "::hx::Object")
  in
  let output_h = h_file#write in
  let def_string = join_class_path class_path "_" in

  gen_class_header ctx tcpp_class h_file scriptable [ (Printf.sprintf "public %s" parent) ];

  Printf.sprintf "\t\ttypedef %s super;\n" super |> output_h;
  Printf.sprintf "\t\ttypedef %s OBJ_;\n" class_name |> output_h;

  let classIdTxt = Printf.sprintf "0x%08lx" tcpp_class.tcl_id in

  output_h ("\t\t" ^ class_name ^ "();\n");
  output_h "\n\tpublic:\n";
  output_h ("\t\tenum { _hx_ClassId = " ^ classIdTxt ^ " };\n\n");
  output_h ("\t\tvoid __construct(" ^ constructor_type_args ^ ");\n");
  Printf.sprintf "\t\tinline void *operator new(size_t inSize, bool inContainer=%b, const char* inName=%s)\n" (Option.is_some tcpp_class.tcl_container) gcName |> output_h;
  output_h
    "\t\t\t{ return ::hx::Object::operator new(inSize,inContainer,inName); }\n";
  output_h "\t\tinline void *operator new(size_t inSize, int extra)\n";
  Printf.sprintf "\t\t\t{ return ::hx::Object::operator new(inSize + extra, %b, %s); }\n" (Option.is_some tcpp_class.tcl_container) gcName |> output_h;
  if has_class_flag class_def CAbstract then output_h "\n"
  else if
    can_inline_constructor base_ctx class_def
  then (
    output_h "\n";
    CppGen.generate_constructor ctx
      (fun str -> output_h ("\t\t" ^ str))
      tcpp_class true)
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
    output_h "\t\tstatic ::Dynamic __CreateEmpty();\n";
    output_h "\t\tstatic ::Dynamic __Create(::hx::DynamicArray inArgs);\n");
  if List.length (tcpp_class.tcl_dynamic_functions) > 0 then
    output_h
      ("\t\tstatic void __alloc_dynamic_functions(::hx::Ctx *_hx_alloc," ^ class_name ^ " *_hx_obj);\n");
  if scriptable then
    output_h "\t\tstatic ::hx::ScriptFunction __script_construct;\n";
  output_h ("\t\t//~" ^ class_name ^ "();\n\n");
  output_h "\t\tHX_DO_RTTI_ALL;\n";
  if has_tcpp_class_flag tcpp_class MemberGet then
    output_h
      "\t\t::hx::Val __Field(const ::String &inString, ::hx::PropertyAccess inCallProp);\n";
  if has_tcpp_class_flag tcpp_class StaticGet then
    output_h
      "\t\tstatic bool __GetStatic(const ::String &inString, ::Dynamic &outValue, ::hx::PropertyAccess inCallProp);\n";
  if has_tcpp_class_flag tcpp_class MemberSet then
    output_h
      "\t\t::hx::Val __SetField(const ::String &inString,const ::hx::Val &inValue, ::hx::PropertyAccess inCallProp);\n";
  if has_tcpp_class_flag tcpp_class StaticSet then
    output_h
      "\t\tstatic bool __SetStatic(const ::String &inString, ::Dynamic &ioValue, ::hx::PropertyAccess inCallProp);\n";
  if has_tcpp_class_flag tcpp_class GetFields then
    output_h
      "\t\tvoid __GetFields(::Array< ::String> &outFields);\n";
  if has_tcpp_class_flag tcpp_class Compare then
    output_h
      ("\t\tint __Compare(const ::hx::Object *inRHS) const { "
      ^ "return const_cast<" ^ class_name
      ^ " *>(this)->__compare(::Dynamic((::hx::Object *)inRHS)); }\n");

  output_h "\t\tstatic void __register();\n";
  if tcpp_class.tcl_container = Some Current then (
    output_h "\t\tvoid __Mark(HX_MARK_PARAMS);\n";
    output_h "\t\tvoid __Visit(HX_VISIT_PARAMS);\n");

  if List.length tcpp_class.tcl_native_interfaces > 0 then (
    output_h "\n\t\tHX_NATIVE_IMPLEMENTATION\n";

    tcpp_class.tcl_native_interfaces
    |> CppGen.needed_interface_functions tcpp_class.tcl_functions
    |> List.iter (fun func ->
      let retVal   = type_to_string func.iff_return in
      let ret      = if retVal = "void" then "" else "return " in
      let argNames = List.map (fun (name, _, _) -> name) func.iff_args in
      output_h
        ("\t\t" ^ retVal ^ " " ^ func.iff_name ^ "( " ^ print_tfun_arg_list true func.iff_args ^ ") {\n");
      output_h
        ("\t\t\t" ^ ret ^ "super::" ^ func.iff_name ^ "( " ^ String.concat "," argNames ^ ");\n\t\t}\n"));

    output_h "\n");

  output_h "\t\tbool _hx_isInstanceOf(int inClassId);\n";
  if List.length tcpp_class.tcl_haxe_interfaces > 0 then (
    output_h "\t\tvoid *_hx_getInterface(int inHash);\n";
    (* generate header glue *)
    let alreadyGlued = Hashtbl.create 0 in
    List.iter
      (fun src ->
        let rec check_interface (interface:tcpp_interface) =
          let check_field func =
            let cast = cpp_tfun_signature false func.iff_args func.iff_return in
            let class_implementation = find_class_implementation func tcpp_class
            in
            let realName = cpp_member_name_of func.iff_field in
            let castKey = realName ^ "::" ^ cast in
            let castKey = match interface.if_class.cl_path with
            | ([ "haxe" ], "IMap") when realName = "set" ->
              castKey ^ "*"
            | _ ->
              castKey
            in
            let implementationKey =
              realName ^ "::" ^ class_implementation
            in
            if castKey <> implementationKey then
              let glue = Printf.sprintf "%s_%08lx" func.iff_field.cf_name (gen_hash32 0 cast) in
              if not (Hashtbl.mem alreadyGlued castKey) then (
                Hashtbl.replace alreadyGlued castKey ();
                let argList = print_tfun_arg_list true func.iff_args in
                let returnType = type_to_string func.iff_return in
                let headerCode = "\t\t" ^ returnType ^ " " ^ glue ^ "(" ^ argList ^ ");\n" in
                output_h headerCode;
                output_h "\n")
          in
          (match interface.if_extends with
          | Some super -> check_interface super
          | _ -> ());
          List.iter check_field interface.if_functions
        in
        check_interface src)
        tcpp_class.tcl_haxe_interfaces);

  if Option.is_some tcpp_class.tcl_init then output_h "\t\tstatic void __init__();\n\n";
  output_h
    ("\t\t::String __ToString() const { return " ^ strq smart_class_name ^ "; }\n\n");

  if has_tcpp_class_flag tcpp_class Boot then output_h "\t\tstatic void __boot();\n";

  tcpp_class.tcl_static_functions
  |> List.iter (gen_member_function ctx class_def true);

  tcpp_class.tcl_static_dynamic_functions
  |> List.iter (gen_dynamic_function ctx class_def true);

  tcpp_class.tcl_static_variables
  |> List.iter (gen_member_variable ctx class_def true);

  tcpp_class.tcl_functions
  |> List.iter (gen_member_function ctx class_def false);

  tcpp_class.tcl_dynamic_functions
  |> List.iter (gen_dynamic_function ctx class_def false);

  tcpp_class.tcl_variables
  |> List.iter (fun field -> gen_member_variable ctx class_def false field);

  output_h (get_class_code class_def Meta.HeaderClassCode);
  output_h "};\n\n";

  end_namespace output_h class_path;

  end_header_file output_h def_string;

  h_file#close
