open Type
open Ast
open CppError

let is_marshalling_managed_class cls =
  has_class_flag cls CExtern && has_meta Meta.CppManagedType cls.cl_meta

let is_marshalling_native_enum a =
  a.a_enum && a.a_extern && has_meta Meta.CppValueType a.a_meta

let is_marshalling_native_value_class cls =
  has_class_flag cls CExtern && has_meta Meta.CppValueType cls.cl_meta

let is_stack_only_marshalling_native_value_class cls =
  if is_marshalling_native_value_class cls then
    let get_meta_field field =
      match Meta.get Meta.CppValueType cls.cl_meta with
      | _, [ (EObjectDecl decls, _) ], _ ->  
        List.find_opt (fun ((n, _, _), _) -> n = field) decls
      | _ ->
        None
    in
    let flag_error pos =
      cpp_abort InvalidFlagsField pos
    in
    let flags =
      match get_meta_field "flags" with
      | Some (_, (EArrayDecl decls, _) ) ->
        decls |> List.filter_map (fun (e, pos) -> match e with | EConst (Ident c) -> Some c | _ -> flag_error pos)
      | Some ((_, pos, _), _) ->
        flag_error pos
      | _ ->
        []
      in
    List.exists (fun v -> v = "StackOnly") flags
  else
    false

let is_marshalling_native_pointer cls =
  has_class_flag cls CExtern && has_meta Meta.CppPointerType cls.cl_meta

let is_marshalling_native_value_class_tvar tvar =
  match follow tvar.v_type with
  | TInst (cls, _) ->
   is_marshalling_native_value_class cls
  | _ ->
    false