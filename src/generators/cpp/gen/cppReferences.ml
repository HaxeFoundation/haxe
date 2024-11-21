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

(*
   Get a list of all classes referred to by the class/enum definition
   These are used for "#include"ing the appropriate header files,
   or for building the dependencies in the Build.xml file
*)
let find_referenced_types_flags ctx obj field_name super_deps constructor_deps header_only for_depends include_super_args =
  let types = ref PMap.empty in
  (if for_depends then
     let include_files =
       get_all_meta_string_path (t_infos obj).mt_meta Meta.Depend
     in
     let include_adder inc =
       types := PMap.add (path_of_string inc) true !types
     in
     List.iter include_adder include_files);
  let rec add_type_flag isNative in_path =
    if not (PMap.mem in_path !types) then (
      types := PMap.add in_path isNative !types;
      try List.iter (add_type_flag isNative) (Hashtbl.find super_deps in_path)
      with Not_found -> ())
  and add_type in_path = add_type_flag false in_path in
  let add_extern_type decl =
    let tinfo = t_infos decl in
    let include_files =
      get_all_meta_string_path tinfo.mt_meta
        (if for_depends then Meta.Depend else Meta.Include)
    in
    if List.length include_files > 0 then
      List.iter (fun inc -> add_type (path_of_string inc)) include_files
    else if (not for_depends) && Meta.has Meta.Include tinfo.mt_meta then
      add_type tinfo.mt_path
  in

  let add_extern_class klass = add_extern_type (TClassDecl klass) in
  let add_extern_enum enum = add_extern_type (TEnumDecl enum) in
  let add_native_gen_class klass =
    let include_files =
      get_all_meta_string_path klass.cl_meta
        (if for_depends then Meta.Depend else Meta.Include)
    in
    if List.length include_files > 0 then
      List.iter (fun inc -> add_type (path_of_string inc)) include_files
    else if for_depends then add_type klass.cl_path
    else
      let path = klass.cl_path in
      if not (has_class_flag klass CInterface) then
        (* Always include native struct headers directly ... *)
        add_type (path_of_string (join_class_path path "/" ^ ".h"))
      else add_type_flag true klass.cl_path
  in
  let visited = ref [] in
  let rec visit_type in_type =
    if not (List.exists (fun t2 -> Type.fast_eq in_type t2) !visited) then (
      visited := in_type :: !visited;
      (match follow in_type with
      | TMono r -> ( match r.tm_type with None -> () | Some t -> visit_type t)
      | TEnum (enum, _) -> (
          match is_extern_enum enum with
          | true -> add_extern_enum enum
          | false -> add_type enum.e_path)
      (* If a class has a template parameter, then we treat it as dynamic - except
         for the Array, Class, FastIterator or Pointer classes, for which we do a fully typed object *)
      | TInst (klass, params) -> (
          match klass.cl_path with
          | [], "Array"
          | [], "Class"
          | [ "cpp" ], "FastIterator"
          | [ "cpp" ], "Pointer"
          | [ "cpp" ], "ConstPointer"
          | [ "cpp" ], "Function"
          | [ "cpp" ], "RawPointer"
          | [ "cpp" ], "RawConstPointer" ->
              List.iter visit_type params
          | _ when is_native_gen_class klass -> add_native_gen_class klass
          | _ when is_extern_class klass ->
              add_extern_class klass;
              List.iter visit_type params
          | _ -> (
              match klass.cl_kind with
              | KTypeParameter _ -> ()
              | _ -> add_type klass.cl_path))
      | TAbstract (a, params) when is_scalar_abstract a ->
          add_extern_type (TAbstractDecl a)
      | TFun (args, haxe_type) ->
          visit_type haxe_type;
          List.iter (fun (_, _, t) -> visit_type t) args
      | _ -> ());
      visited := List.tl !visited)
  in
  let visit_params expression =
    let rec visit_expression expression =
      (* Expand out TTypeExpr (ie, the name of a class, as used for static access etc ... *)
      (match expression.eexpr with
      | TTypeExpr type_def -> (
          match type_def with
          | TClassDecl class_def when is_native_gen_class class_def ->
              add_native_gen_class class_def
          | TClassDecl class_def when is_extern_class class_def ->
              add_extern_class class_def
          | TEnumDecl enum_def when is_extern_enum enum_def ->
              add_extern_enum enum_def
          | _ -> add_type (t_path type_def))
      (* Must visit the types, Type.iter will visit the expressions ... *)
      | TTry (e, catches) ->
          List.iter (fun (v, _) -> visit_type v.v_type) catches
      (* Must visit type too, Type.iter will visit the expressions ... *)
      | TNew (klass, params, _) -> (
          visit_type (TInst (klass, params));
          try
            let construct_type = Hashtbl.find constructor_deps klass.cl_path in
            visit_type construct_type.cf_type
          with Not_found -> ())
      (* Must visit type too, Type.iter will visit the expressions ... *)
      | TVar (v, _) -> visit_type v.v_type
      (* Must visit enum type too, Type.iter will visit the expressions ... *)
      | TEnumParameter (_, ef, _) -> visit_type (follow ef.ef_type)
      (* Must visit args too, Type.iter will visit the expressions ... *)
      | TFunction func_def ->
          List.iter (fun (v, _) -> visit_type v.v_type) func_def.tf_args
      | TField (obj, field) -> (
          match field with
          | FInstance (clazz, params, _) | FClosure (Some (clazz, params), _) ->
              visit_type (TInst (clazz, params))
          | _ -> ())
      | TConst TSuper -> (
          match follow expression.etype with
          | TInst (klass, params) -> (
              try
                let construct_type =
                  Hashtbl.find constructor_deps klass.cl_path
                in
                visit_type construct_type.cf_type
              with Not_found -> ())
          | _ ->
              print_endline
                ("TSuper : Odd etype ?"
                ^ (CppRetyper.cpp_type_of expression.etype |> tcpp_to_string)))
      | _ -> ());
      Type.iter visit_expression expression;
      visit_type (follow expression.etype)
    in
    visit_expression expression
  in
  let visit_field field =
    (* Add the type of the expression ... *)
    visit_type field.cf_type;
    if not header_only then
      match field.cf_expr with
      | Some expression -> visit_params expression
      | _ -> ()
  in
  let visit_class class_def =
    let fields =
      List.append class_def.cl_ordered_fields class_def.cl_ordered_statics
    in
    let fields_and_constructor =
      List.append fields
        (match class_def.cl_constructor with Some expr -> [ expr ] | _ -> [])
    in
    let fields_and_constructor =
      if field_name = "*" then fields_and_constructor
      else List.filter (fun f -> f.cf_name = field_name) fields_and_constructor
    in
    List.iter visit_field fields_and_constructor;
    if include_super_args then
      List.iter visit_field
        (List.map (fun (a, _, _) -> a) (all_virtual_functions class_def));

    (* Add super & interfaces *)
    if is_native_gen_class class_def then add_native_gen_class class_def
    else add_type class_def.cl_path
  in
  let visit_enum enum_def =
    add_type enum_def.e_path;
    PMap.iter
      (fun _ constructor ->
        match constructor.ef_type with
        | TFun (args, _) -> List.iter (fun (_, _, t) -> visit_type t) args
        | _ -> ())
      enum_def.e_constrs;
    if not header_only then
      let meta =
        Texpr.build_metadata ctx.ctx_common.basic (TEnumDecl enum_def)
      in
      match meta with Some expr -> visit_params expr | _ -> ()
  in
  let inc_cmp i1 i2 =
    String.compare (join_class_path i1 ".") (join_class_path i2 ".")
  in

  (* Body of main function *)
  (match obj with
  | TClassDecl class_def -> (
      visit_class class_def;
      match TClass.get_cl_init class_def with
      | Some expression -> visit_params expression
      | _ -> ())
  | TEnumDecl enum_def -> visit_enum enum_def
  | TTypeDecl _ | TAbstractDecl _ -> (* These are expanded *) ());

  (*
        The internal header files are also defined in the hx/Object.h file, so you do
        #include them separately. However, Math classes has its
        own header file (under the hxcpp tree) so these should be included
    *)
  let include_class_header = function
    | [], "@Main" -> false
    | [], "Math" -> true
    | path -> not (is_internal_class path)
  in
  let deps =
    List.sort inc_cmp
      (List.filter (fun path -> include_class_header path) (pmap_keys !types))
  in
  let flags = List.map (fun dep -> PMap.find dep !types) deps in
  (deps, flags)

let find_referenced_types ctx obj super_deps constructor_deps header_only for_depends include_super_args =
  let deps, _ =
    find_referenced_types_flags ctx obj "*" super_deps constructor_deps
      header_only for_depends include_super_args
  in
  deps
