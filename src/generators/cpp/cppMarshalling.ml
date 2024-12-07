open Ast
open Type
open Error
open Common
open Globals
open CppStrings
open CppTypeUtils
open CppAst
open CppAstTools
open CppSourceWriter
open CppContext

let get_extern_value_type_struct cls =
  Printf.sprintf "::cpp::Struct< %s >" (get_extern_value_type cls)

let get_extern_value_type_reference cls =
  Printf.sprintf "::cpp::Reference< %s >" (get_extern_value_type cls)

let type_to_string t =
  match follow_lazy_and_mono t with
  | TInst (cls, _) when has_class_flag cls CExtern && has_meta Meta.CppValueType cls.cl_meta ->
    get_extern_value_type_struct cls
  (* | TType ({ t_path = ([ "cpp" ], "Reference") }, [ TInst (cls, _) ]) when has_class_flag cls CExtern && has_meta Meta.CppValueType cls.cl_meta ->
    get_extern_value_type_reference cls *)
  | _ ->
    tcpp_to_string (CppRetyper.cpp_type_of t)
(* 
let retype_marshalling_class class_def =
  let filter_field field =
    let find_marshalling_for arg =
      let finder (meta, exprs, _) =
        match meta with
        | Meta.CppMarshal ->
          (match exprs with
          | ((EConst(Ident s)),_)::rest when s = arg -> Some rest
          | _ -> None)
        | _ -> None
      in

      List.find_map finder field.cf_meta
    in

    match field.cf_kind, field.cf_type with
    | (Method MethNormal, TFun func) when has_meta Meta.CppMarshal field.cf_meta ->
      ()
    | _ ->
      ()
    in
  () *)