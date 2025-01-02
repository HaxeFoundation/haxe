open Ast
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

let gen_function ctx class_def class_name is_static func =
  let output          = ctx.ctx_output in
  let return_type_str = type_to_string func.tcf_func.tf_type in
  let return_type     = cpp_type_of func.tcf_func.tf_type in
  let is_void         = return_type = TCppVoid in
  let ret             = if is_void then "(void)" else "return " in
  let needsWrapper t =
    match t with
    | TCppStar _ -> true
    | TCppInst (t, _) -> Meta.has Meta.StructAccess t.cl_meta
    | _ -> false
  in

  (* The actual function definition *)
  output (if is_void then "void" else return_type_str);
  output (" " ^ class_name ^ "::" ^ func.tcf_name ^ "(");
  output (print_arg_list func.tcf_func.tf_args "__o_");
  output ")";
  ctx.ctx_real_this_ptr <- true;
  let code = get_code func.tcf_field.cf_meta Meta.FunctionCode in
  let tail_code = get_code func.tcf_field.cf_meta Meta.FunctionTailCode in

  match get_meta_string func.tcf_field.cf_meta Meta.Native with
  | Some nativeImpl when is_static ->
    output " {\n";
    output
      ("\t" ^ ret ^ "::" ^ nativeImpl ^ "("
      ^ print_arg_list_name func.tcf_func.tf_args "__o_"
      ^ ");\n");
    output "}\n\n"
  | _ ->
    with_debug
      ctx
      func.tcf_field.cf_meta
      (gen_cpp_function_body ctx class_def is_static func.tcf_field.cf_name func.tcf_func code tail_code);

    output "\n\n";
    
    (* generate dynamic version too ... *)
    if (not func.tcf_is_virtual || not func.tcf_is_overriding) && func.tcf_is_reflective then
      let tcpp_args = List.map (fun (v, _) -> cpp_type_of v.v_type) func.tcf_func.tf_args in
      let wrap      = needsWrapper return_type || List.exists needsWrapper tcpp_args in

      if wrap then (
        let wrapName = "_hx_wrap" ^ class_name ^ "_" ^ func.tcf_name in
        output ("static ::Dynamic " ^ wrapName ^ "( ");

        let initial = if is_static then [] else [ "::hx::Object *obj" ] in

        initial @ (List.init (List.length tcpp_args) (fun idx -> Printf.sprintf "const ::Dynamic &a%i" idx))
        |> String.concat ","
        |> output;

        output ") {\n\t";
        (if not is_void then
            match return_type with
            | TCppStar _ -> output "return (cpp::Pointer<const void *>) "
            | TCppInst (t, _) when Meta.has Meta.StructAccess t.cl_meta
              ->
                output ("return (cpp::Struct< " ^ tcpp_to_string return_type ^ " >) ")
            | _ -> output "return ");

        if is_static then
          output (class_name ^ "::" ^ func.tcf_name ^ "(")
        else
          output ("reinterpret_cast< " ^ class_name ^ " *>(obj)->" ^ func.tcf_name ^ "(");

        let cast_prefix idx arg =
          match arg with
          | TCppStar (t, const) ->
              Printf.sprintf "(::cpp::%sPointer< %s >) a%i" (if const then "Const" else "") (tcpp_to_string t) idx
          | TCppInst (t, _) when Meta.has Meta.StructAccess t.cl_meta ->
            Printf.sprintf "(::cpp::Struct< %s >) a%i" (tcpp_to_string arg) idx
          | _ ->
            Printf.sprintf "a%i" idx in
            
        tcpp_args
        |> ExtList.List.mapi cast_prefix
        |> String.concat ", "
        |> output;

        output ");\n";

        if is_void then output "\treturn null();\n";
        output "}\n";
        let nName = string_of_int (List.length tcpp_args) in
        output
          ("::Dynamic " ^ class_name ^ "::" ^ func.tcf_name ^ "_dyn() {\n\treturn ");
        if is_static then
          output
            ("::hx::CreateStaticFunction" ^ nName ^ "(\"" ^ func.tcf_name ^ "\"," ^ wrapName ^ ");")
        else
          output
            ("::hx::CreateMemberFunction" ^ nName ^ "(\"" ^ func.tcf_name ^ "\",this," ^ wrapName ^ ");");
        output "}\n")
      else
        let prefix = if is_static then "STATIC_" else "" in
        Printf.sprintf "%sHX_DEFINE_DYNAMIC_FUNC%i(%s, %s, %s)\n\n" prefix (List.length func.tcf_func.tf_args) class_name func.tcf_name ret |> output

let gen_dynamic_function ctx class_def class_name is_static is_for_static_var (func:tcpp_class_function) =
  let output = ctx.ctx_output in
  let func_name = "__default_" ^ func.tcf_name in
  let nargs = string_of_int (List.length func.tcf_func.tf_args) in
  let return_type_str = type_to_string func.tcf_func.tf_type in
  let return_type = cpp_type_of func.tcf_func.tf_type in
  let no_debug = Meta.has Meta.NoDebug func.tcf_field.cf_meta in
  let is_void = return_type = TCppVoid in
  let ret = if is_void then "(void)" else "return " in

  ctx.ctx_real_this_ptr <- false;
  Printf.sprintf "HX_BEGIN_DEFAULT_FUNC(%s, %s)\n" func_name class_name |> output; 
  Printf.sprintf "%s _hx_run(%s)" return_type_str (print_arg_list func.tcf_func.tf_args "__o_") |> output;

  gen_cpp_function_body ctx class_def is_static func_name func.tcf_func "" "" no_debug;

  output ("HX_END_LOCAL_FUNC" ^ nargs ^ "(" ^ ret ^ ")\n");
  output "HX_END_DEFAULT_FUNC\n\n"

let gen_static_variable ctx class_def class_name (var:tcpp_class_variable) =
  let output = ctx.ctx_output in
  Printf.sprintf "%s %s::%s;\n\n" (type_to_string var.tcv_type) class_name var.tcv_name |> output

let gen_dynamic_function_init ctx class_def func =
  match func.tcf_field.cf_expr with
  | Some { eexpr = TFunction function_def } ->
    Printf.sprintf "\t%s = new %s;\n\n" func.tcf_name ("__default_" ^ func.tcf_name) |> ctx.ctx_output
  | _ ->
    ()

let gen_var_init ctx class_def var =
  match var.tcv_field.cf_expr with
  | Some expr ->
    gen_cpp_init ctx (join_class_path class_def.cl_path ".") "boot" (var.tcv_name ^ " = ") expr
  | _ -> ()

let gen_boot_field ctx output_cpp tcpp_class =
  if has_tcpp_class_flag tcpp_class Boot then (
    output_cpp ("void " ^ tcpp_class.tcl_name ^ "::__boot()\n{\n");

    let dot_name = join_class_path tcpp_class.tcl_class.cl_path "." in

    (match tcpp_class.tcl_meta with
    | Some expr -> gen_cpp_init ctx dot_name "boot" "__mClass->__meta__ = " expr
    | None -> ());

    (match tcpp_class.tcl_rtti with
    | Some expr -> gen_cpp_init ctx dot_name "boot" "__mClass->__rtti__ = " expr
    | None -> ());

    List.iter (gen_var_init ctx tcpp_class.tcl_class) tcpp_class.tcl_static_variables;
    List.iter (gen_dynamic_function_init ctx tcpp_class.tcl_class) tcpp_class.tcl_static_dynamic_functions;

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
    let mapper func =
      Printf.sprintf "\tif (!_hx_obj->%s.mPtr) { _hx_obj->%s = new __default_%s(_hx_obj); }" func.tcf_name func.tcf_name func.tcf_name in
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

let print_reflective_fields ctx_common class_def variables functions =
  let filter_vars var acc =
    if var.tcv_is_reflective then
      Printf.sprintf "\t%s" (strq ctx_common var.tcv_field.cf_name) :: acc
    else
      acc in
  let filter_funcs func acc =
    if func.tcf_is_reflective then
      Printf.sprintf "\t%s" (strq ctx_common func.tcf_field.cf_name) :: acc
    else
      acc in

  let calls =
    [ "\t::String(null())" ]
    |> List.fold_right filter_vars variables
    |> List.fold_right filter_funcs functions
  in

  if List.length calls > 1 then
    Some (String.concat ",\n" calls)
  else
    None

let cpp_interface_impl_name cls =
  "_hx_" ^ join_class_path cls.cl_path "_"

let generate_native_class base_ctx tcpp_class =
  let class_def = tcpp_class.tcl_class in
  let class_path = class_def.cl_path in
  let debug = tcpp_class.tcl_debug_level in
  let cpp_file = new_placed_cpp_file base_ctx.ctx_common class_path in
  let cpp_ctx = file_context base_ctx cpp_file debug false in
  let ctx = cpp_ctx in
  let output_cpp = cpp_file#write in
  let scriptable = has_tcpp_class_flag tcpp_class Scriptable in

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
  let rec parent_id_folder acc cur =
    match cur.tcl_super with
    | Some s -> parent_id_folder (cur.tcl_id :: acc) s
    | None -> cur.tcl_id :: acc
  in
  let implemented_classes =
    tcpp_class
    |> parent_id_folder [ Int32.of_int 1 ]
    |> List.sort compare
  in
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

  if List.length tcpp_class.tcl_haxe_interfaces > 0 then (
    let cname     = cpp_interface_impl_name class_def in
    let impl_name = cpp_class_name class_def in

    let fold_interface (glued, acc) interface =
    
      let rec gen_interface_funcs interface =

        let fold_field (glued, acc) func =
          let cast      = cpp_tfun_signature false func.iff_args func.iff_return in
          let real_name = cpp_member_name_of func.iff_field in
          
          (* C++ can't work out which function it needs to take the addrss of
              when the implementation is overloaded - currently the map-set functions.
              Change the castKey to force a glue function in this case (could double-cast the pointer, but it is ugly)
          *)
          let suffix =
            match interface.if_class.cl_path with
            | (["haxe"], "IMap") when real_name = "set" -> "*"
            | _ -> "" in
          let cast_key = Printf.sprintf "%s::%s%s" real_name cast suffix in
          let implementation_key = Printf.sprintf "%s::%s" real_name (find_class_implementation func tcpp_class) in

          if cast_key = implementation_key then
            (glued, Printf.sprintf "\t%s&%s::%s" cast impl_name real_name :: acc)
          else
            let glue  = Printf.sprintf "%s_%08lx" func.iff_field.cf_name (gen_hash32 0 cast) in
            let glued =
              if StringMap.mem cast_key glued then
                glued
              else
                let arg_list    = print_tfun_arg_list true func.iff_args in
                let return_type = type_to_string func.iff_return in
                let return_str  = if return_type = "void" then "" else "return " in
                let cpp_code    =
                  Printf.sprintf
                    "%s %s::%s(%s) { %s%s(%s); }\n"
                    return_type
                    class_name
                    glue
                    arg_list
                    return_str
                    real_name
                    (print_arg_names func.iff_args) in
                StringMap.add cast_key cpp_code glued
            in
            (glued, Printf.sprintf "\t%s&%s::%s" cast impl_name glue :: acc)
        in

        let initial =
          match interface.if_extends with
          | Some super -> gen_interface_funcs super
          | _ -> (glued, [])
        in
        List.fold_left fold_field initial interface.if_functions
      in

      let interface_name = cpp_interface_impl_name interface.if_class in
      let glued, funcs   = gen_interface_funcs interface in
      let combined = funcs |> List.rev |> String.concat ",\n" in
      let call     = Printf.sprintf "static %s %s_%s = {\n%s\n};\n" (cpp_class_name interface.if_class) cname interface_name combined in
      (glued, call :: acc)
    in
    
    let glued, calls =
      List.fold_left
        fold_interface
        (StringMap.empty, [])
        tcpp_class.tcl_haxe_interfaces in

    calls |> String.concat "\n" |> output_cpp;
    glued |> StringMap.bindings |> List.map snd |> String.concat "\n" |> output_cpp;

    output_cpp ("void *" ^ class_name ^ "::_hx_getInterface(int inHash) {\n");
    output_cpp "\tswitch(inHash) {\n";

    let iter interface =
      output_cpp ("\t\tcase (int)" ^ interface.if_hash ^ ": return &" ^ cname ^ "_" ^ cpp_interface_impl_name interface.if_class ^ ";\n") in
    List.iter
      iter
      tcpp_class.tcl_haxe_interfaces;

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

  List.iter (gen_function ctx class_def class_name false) tcpp_class.tcl_functions;
  List.iter (gen_dynamic_function ctx class_def class_name false false) tcpp_class.tcl_dynamic_functions;

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

  (* Initialise non-static variables *)
  output_cpp (class_name ^ "::" ^ class_name ^ "()\n{\n");
  List.iter
    (fun func -> output_cpp ("\t" ^ func.tcf_name ^ " = new __default_" ^ func.tcf_name ^ "(this);\n"))
    tcpp_class.tcl_dynamic_functions;
  output_cpp "}\n\n";

  if tcpp_class.tcl_container = Some Current then (
    let rec find_next_super_iteration cls =
      match cls.tcl_super with
      | Some ({ tcl_container = Some Current } as super) ->
        Some (tcpp_to_string_suffix "_obj" (cpp_instance_type super.tcl_class super.tcl_params))
      | Some super ->
        find_next_super_iteration super
      | None ->
        None
    in

    let super_needs_iteration = find_next_super_iteration tcpp_class in
    let smart_class_name = snd class_path in
    let dump_field_iterator macro var =
      Printf.sprintf "\t%s(%s, \"%s\");\n" macro var.tcv_name var.tcv_field.cf_name |> output_cpp
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
      || Gctx.defined common_ctx Define.ForceNativeProperty
    then "inCallProp != ::hx::paccNever"
    else "inCallProp == ::hx::paccAlways"
  in

  let get_wrapper field value =
    match cpp_type_of field.cf_type with
    | TCppInst (t, _) as inst when Meta.has Meta.StructAccess t.cl_meta ->
      Printf.sprintf "(::cpp::Struct< %s >) %s" (tcpp_to_string inst) value
    | TCppStar _ ->
      Printf.sprintf "(::cpp::Pointer<void*>) %s" value
    | _ ->
      value
    in

  let print_variable var_printer get_printer (var:tcpp_class_variable) acc =
    if var.tcv_is_reflective && not (is_abstract_impl class_def) then
      let variable = get_wrapper var.tcv_field var.tcv_name in

      match var.tcv_field.cf_kind with
      | Var { v_read = AccCall } ->
        let prop_check = checkPropCall var.tcv_field in
        let getter     = Printf.sprintf "get_%s()" var.tcv_field.cf_name |> get_wrapper var.tcv_field in

        (var.tcv_field.cf_name, String.length var.tcv_field.cf_name, get_printer prop_check getter variable) :: acc
      | _ ->
        (var.tcv_field.cf_name, String.length var.tcv_field.cf_name, var_printer variable) :: acc
    else
      acc
  in

  let print_function printer func acc =
    if func.tcf_is_reflective then
      let ident  = get_wrapper func.tcf_field func.tcf_name |> printer in
      let length = String.length func.tcf_field.cf_name in

      (func.tcf_field.cf_name, length, ident) :: acc
    else
      acc
  in

  let print_property printer (var:tcpp_class_variable) acc =
    if var.tcv_is_reflective && not (is_abstract_impl class_def) then
      let prop_check = checkPropCall var.tcv_field in
      let getter     = Printf.sprintf "get_%s()" var.tcv_field.cf_name |> get_wrapper var.tcv_field in
      (var.tcv_field.cf_name, String.length var.tcv_field.cf_name, printer prop_check getter) :: acc
    else
      acc
  in
  
  let castable f =
    match cpp_type_of f.cf_type with
    | TCppInst (t, _) as inst when Meta.has Meta.StructAccess t.cl_meta ->
        "cpp::Struct< " ^ tcpp_to_string inst ^ " > "
    | TCppStar (t, _) -> "cpp::Pointer< " ^ tcpp_to_string t ^ " >"
    | _ -> type_to_string f.cf_type
  in

  if has_tcpp_class_flag tcpp_class MemberGet then (
    (* Dynamic "Get" Field function - string version *)
    Printf.sprintf "::hx::Val %s::__Field(const ::String &inName,::hx::PropertyAccess inCallProp)\n{\n" class_name |> output_cpp;

    let var_printer ident = Printf.sprintf "return ::hx::Val( %s );" ident in
    let get_printer check getter ident = Printf.sprintf "return ::hx::Val( %s ? %s : %s );" check getter ident in
    let fun_printer ident = Printf.sprintf "return ::hx::Val( %s_dyn() );" ident in
    let prop_printer check ident = Printf.sprintf "if (%s) { return ::hx::Val( %s ); }" check ident in

    let all_fields = []
      |> List.fold_right (print_variable var_printer get_printer) tcpp_class.tcl_variables
      |> List.fold_right (print_property prop_printer) tcpp_class.tcl_properties
      |> List.fold_right (print_function fun_printer) tcpp_class.tcl_functions in

    if List.length all_fields > 0 then (
      dump_quick_field_test all_fields;
      output_cpp "\treturn super::__Field(inName,inCallProp);\n}\n\n");
    );

  if has_tcpp_class_flag tcpp_class StaticGet then (
    Printf.sprintf "bool %s::__GetStatic(const ::String &inName, ::Dynamic &outValue, ::hx::PropertyAccess inCallProp)\n{\n" class_name |> output_cpp;

    let var_printer ident = Printf.sprintf "outValue = %s; return true;" ident in
    let get_printer check getter ident = Printf.sprintf "outValue = %s ? %s : %s; return true;" check getter ident in
    let fun_printer ident = Printf.sprintf "outValue = %s_dyn(); return true;" ident in
    let prop_printer check ident = Printf.sprintf "if (%s) { outValue = %s; return true; }" check ident in

    let all_fields = []
      |> List.fold_right (print_variable var_printer get_printer) tcpp_class.tcl_static_variables
      |> List.fold_right (print_property prop_printer) tcpp_class.tcl_static_properties
      |> List.fold_right (print_function fun_printer) tcpp_class.tcl_static_functions in

    dump_quick_field_test all_fields;
    output_cpp "\treturn false;\n}\n\n");

  if has_tcpp_class_flag tcpp_class MemberSet then (
    Printf.sprintf "::hx::Val %s::__SetField(const ::String& inName, const ::hx::Val& inValue, ::hx::PropertyAccess inCallProp)\n{\n" class_name |> output_cpp;

    let fold_variable (var:tcpp_class_variable) acc =
      if var.tcv_is_reflective && not (is_abstract_impl class_def) then
        let casted  = castable var.tcv_field in
        let default = if var.tcv_is_gc_element then
          Printf.sprintf "_hx_set_%s(HX_CTX_GET, inValue.Cast< %s >()); return inValue;" var.tcv_name casted
        else
          Printf.sprintf "%s = inValue.Cast< %s >(); return inValue;" var.tcv_name casted in

        match var.tcv_field.cf_kind with
        | Var { v_write = AccCall } ->
          let prop_call = checkPropCall var.tcv_field in
          let setter    = Printf.sprintf "set_%s" var.tcv_field.cf_name |> get_wrapper var.tcv_field in
          let call      = Printf.sprintf "if (%s) { return ::hx::Val( %s(inValue.Cast< %s >()) ); } else { %s }" prop_call setter casted default in

          (var.tcv_field.cf_name, String.length var.tcv_field.cf_name, call) :: acc
        | Var { v_write = AccNormal | AccNo | AccNever } ->
          (var.tcv_field.cf_name, String.length var.tcv_field.cf_name, default) :: acc
        | _ ->
          acc
      else
        acc
    in

    let fold_property (var:tcpp_class_variable) acc =
      if var.tcv_is_reflective && not (is_abstract_impl class_def) then
        let casted  = castable var.tcv_field in

        match var.tcv_field.cf_kind with
        | Var { v_write = AccCall } ->
          let prop_call = checkPropCall var.tcv_field in
          let setter    = Printf.sprintf "set_%s" var.tcv_field.cf_name |> get_wrapper var.tcv_field in
          let call      = Printf.sprintf "if (%s) { return ::hx::Val( %s(inValue.Cast< %s >()) ); }" prop_call setter casted in

          (var.tcv_field.cf_name, String.length var.tcv_field.cf_name, call) :: acc
        | _ ->
          acc
      else
        acc
    in

    let all_fields = []
    |> List.fold_right fold_variable tcpp_class.tcl_variables
    |> List.fold_right fold_property tcpp_class.tcl_properties in

    dump_quick_field_test all_fields;
    output_cpp "\treturn super::__SetField(inName,inValue,inCallProp);\n}\n\n");

  if has_tcpp_class_flag tcpp_class StaticSet then (
    Printf.sprintf "bool %s::__SetStatic(const ::String& inName, ::Dynamic& ioValue, ::hx::PropertyAccess inCallProp)\n{\n" class_name |> output_cpp;

    let fold_variable (var:tcpp_class_variable) acc =
      if var.tcv_is_reflective && not (is_abstract_impl class_def) then
        let casted = castable var.tcv_field in

        match var.tcv_field.cf_kind with
        | Var { v_write = AccCall } ->
          let prop_call = checkPropCall var.tcv_field in
          let setter    = Printf.sprintf "set_%s" var.tcv_field.cf_name |> get_wrapper var.tcv_field in
          let call      = Printf.sprintf "if (%s) { ioValue = %s(ioValue.Cast< %s >()); } else { %s = ioValue.Cast< %s >(); } return true;" prop_call setter casted var.tcv_name casted in

          (var.tcv_field.cf_name, String.length var.tcv_field.cf_name, call) :: acc
        | Var { v_write = AccNormal | AccNo } ->
          (var.tcv_field.cf_name, String.length var.tcv_field.cf_name, Printf.sprintf "%s = ioValue.Cast< %s >(); return true;" var.tcv_name casted) :: acc
        | _ ->
          acc
      else
        acc
    in

    let fold_property (var:tcpp_class_variable) acc =
      if var.tcv_is_reflective && not (is_abstract_impl class_def) then
        match var.tcv_field.cf_kind with
        | Var { v_write = AccCall } ->
          let prop_call = checkPropCall var.tcv_field in
          let setter    = Printf.sprintf "set_%s" var.tcv_field.cf_name |> get_wrapper var.tcv_field in
          let casted    = castable var.tcv_field in

          (var.tcv_field.cf_name, String.length var.tcv_field.cf_name, Printf.sprintf "if (%s) { ioValue = %s(ioValue.Cast< %s >()); }" prop_call setter casted) :: acc
        | _ ->
          acc
      else
        acc
    in

    let all_fields = []
      |> List.fold_right fold_variable tcpp_class.tcl_static_variables
      |> List.fold_right fold_property tcpp_class.tcl_static_properties in

    dump_quick_field_test all_fields;
    output_cpp "\treturn false;\n}\n\n");

  (* For getting a list of data members (eg, for serialization) *)
  if has_tcpp_class_flag tcpp_class GetFields then (

    let append var acc = (strq var.tcv_field.cf_name |> Printf.sprintf "\toutFields->push(%s);") :: acc in
    let fields =
      [ "\tsuper::__GetFields(outFields);" ]
      |> List.fold_right append tcpp_class.tcl_variables
      |> List.fold_right append tcpp_class.tcl_properties
      |> String.concat "\n" in

    Printf.sprintf "void %s::__GetFields(::Array< ::String >& outFields)\n{\n%s\n}\n\n" class_name fields |> output_cpp);

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
  let dump_member_storage (var:tcpp_class_variable) =
    Printf.sprintf
      "\t{ %s, (int)offsetof(%s, %s), %s },\n" (storage var.tcv_field) class_name var.tcv_name (strq var.tcv_field.cf_name) |> output_cpp
  in
  let dump_static_storage (var:tcpp_class_variable) =
    Printf.sprintf "\t{ %s, (void*) &%s::%s, %s },\n" (storage var.tcv_field) class_name var.tcv_name (strq var.tcv_field.cf_name) |> output_cpp
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

  (match print_reflective_fields ctx.ctx_common class_def tcpp_class.tcl_variables tcpp_class.tcl_functions with
  | Some str ->
    Printf.sprintf "static ::String %s_sMemberFields[] = {\n%s\n};\n\n" class_name str |> output_cpp
  | None ->
    Printf.sprintf "static ::String* %s_sMemberFields = 0;\n\n" class_name |> output_cpp);

  if List.length tcpp_class.tcl_static_variables > 0 then (
    let dump_field_iterator macro var =
      Printf.sprintf "\t%s(%s::%s, \"%s\");" macro class_name var.tcv_name var.tcv_field.cf_name
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
    let dump_script_func idx func =
      match func.tcf_field.cf_type with
      | TFun (f_args, _) ->
        let args = print_tfun_arg_list true f_args in
        let return_type = type_to_string func.tcf_func.tf_type in
        let ret = if return_type = "Void" || return_type = "void" then " " else "return " in
        let vtable = Printf.sprintf "__scriptVTable[%i]" (idx + 1) in

        Printf.sprintf "\t%s %s(%s) {\n" return_type func.tcf_name args |> output_cpp;
        Printf.sprintf ("\tif (%s) {\n") vtable |> output_cpp;
        output_cpp "\t\t::hx::CppiaCtx *__ctx = ::hx::CppiaCtx::getCurrent();\n";
        output_cpp "\t\t::hx::AutoStack __as(__ctx);\n";
        output_cpp ("\t\t__ctx->pushObject( this );\n");

        List.iter
          (fun (name, opt, t) ->
            Printf.sprintf "\t\t__ctx->push%s(%s);\n" (CppCppia.script_type t opt) (keyword_remap name) |> output_cpp)
        f_args;

        output_cpp
          ("\t\t" ^ ret ^ "__ctx->run" ^ CppCppia.script_type func.tcf_func.tf_type false ^ "(" ^ vtable ^ ");\n");
        output_cpp ("\t}  else " ^ ret);

        let names = List.map (fun (n, _, _) -> keyword_remap n) f_args in

        output_cpp
          (class_name ^ "::" ^ func.tcf_name ^ "(" ^ String.concat "," names ^ ");");

        if return_type <> "void" then output_cpp "return null();";

        output_cpp "}\n";
      | _ ->
        abort "expected function type to be tfun" func.tcf_field.cf_pos
    in

    let script_name = class_name ^ "__scriptable" in
    let has_funky_toString =
      List.exists
        (fun func -> func.tcf_name = "toString")
        tcpp_class.tcl_static_functions ||
      List.exists
        (fun func -> func.tcf_name = "toString" && List.length func.tcf_func.tf_args <> 0)
        tcpp_class.tcl_functions
    in
    let super_string =
      if has_funky_toString then class_name ^ "::super" else class_name
    in

    Printf.sprintf "class %s : public %s {\n" script_name class_name |> output_cpp;
    Printf.sprintf "\ttypedef %s __ME;\n" script_name |> output_cpp;
    Printf.sprintf "\ttypedef %s super;\n" class_name |> output_cpp;
    Printf.sprintf "\ttypedef %s __superString;\n" super_string |> output_cpp;
    Printf.sprintf "\tHX_DEFINE_SCRIPTABLE(HX_ARR_LIST%i)\n" (List.length constructor_var_list) |> output_cpp;
    output_cpp "\tHX_DEFINE_SCRIPTABLE_DYNAMIC;\n";

    (*
      Functions are added in reverse order (oldest on right), then list is reversed because this is easier in ocaml
      The order is important because cppia looks up functions by index
    *)
    let flatten_tcpp_class_functions =
      let current_virtual_functions_rev cls base_functions =
        let folder result elem =
          if elem.tcf_is_overriding then
            if List.exists (fun f -> f.tcf_name = elem.tcf_name) result then
              result
            else
              elem :: result
          else
            elem :: result
        in

        List.fold_left folder base_functions cls.tcl_functions
      in

      let rec flatten_tcpp_class_functions_rec cls =
        let initial =
          match cls.tcl_super with
          | Some super -> flatten_tcpp_class_functions_rec super
          | _ -> [] in
        current_virtual_functions_rev cls initial
      in
    
      flatten_tcpp_class_functions_rec tcpp_class |> List.rev
    in

    flatten_tcpp_class_functions
    |> List.filter (fun f -> f.tcf_name <> "toString")
    |> ExtList.List.iteri dump_script_func;
    output_cpp "};\n\n";

    if List.length tcpp_class.tcl_functions > 0 || List.length tcpp_class.tcl_static_functions > 0 then (

      let dump_script is_static f acc =
        let signature = generate_script_function is_static f.tcf_field ("__s_" ^ f.tcf_field.cf_name) f.tcf_name in
        let superCall = if is_static then "0" else "__s_" ^ f.tcf_field.cf_name ^ "<true>" in
        let named =
          Printf.sprintf
            "\t::hx::ScriptNamedFunction(\"%s\", __s_%s, \"%s\", %s HXCPP_CPPIA_SUPER_ARG(%s))"
            f.tcf_field.cf_name
            f.tcf_field.cf_name
            signature
            (if is_static then "true" else "false")
            superCall in

        named :: acc
      in

      let sigs =
        [ "\t::hx::ScriptNamedFunction(0,0,0 HXCPP_CPPIA_SUPER_ARG(0) )" ]
        |> List.fold_right (dump_script false) tcpp_class.tcl_functions
        |> List.fold_right (dump_script true) tcpp_class.tcl_static_functions
        |> String.concat ",\n"
      in

      output_cpp "#ifndef HXCPP_CPPIA_SUPER_ARG\n";
      output_cpp "#define HXCPP_CPPIA_SUPER_ARG(x)\n";
      output_cpp "#endif\n";
      Printf.sprintf "static ::hx::ScriptNamedFunction __scriptableFunctions[] = {\n%s\n};\n\n" sigs |> output_cpp)
    else
      output_cpp "static ::hx::ScriptNamedFunction *__scriptableFunctions = 0;\n");

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

  (match print_reflective_fields ctx.ctx_common class_def tcpp_class.tcl_static_variables tcpp_class.tcl_static_functions with
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
    if has_tcpp_class_flag tcpp_class StaticGet then class_name ^ "::__GetStatic;\n"
    else "::hx::Class_obj::GetNoStaticField;\n");
  output_cpp
    ("\t__mClass->mSetStaticField = &"
    ^
    if has_tcpp_class_flag tcpp_class StaticSet then class_name ^ "::__SetStatic;\n"
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
        ^ join_class_path_remap intf_def.if_class.cl_path "::"
        ^ ");\n"))
        tcpp_class.tcl_native_interfaces;
  output_cpp "}\n\n";

  gen_boot_field ctx output_cpp tcpp_class;

  end_namespace output_cpp class_path;

  cpp_file#close
