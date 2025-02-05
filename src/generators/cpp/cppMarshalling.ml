open CppAst
open CppAstTools

let get_extern_value_type_boxed value_type =
  let p = get_native_marshalled_type value_type in
  let suffix =
    match value_type with
    | Pointer _ -> "*"
    | _ -> ""
  in

  Printf.sprintf "::cpp::marshal::Boxed< %s%s >" p suffix, Printf.sprintf "::cpp::marshal::Boxed_obj< %s%s >" p suffix