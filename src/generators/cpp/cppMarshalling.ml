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

let get_extern_value_type_boxed cls params =
  let p = get_extern_value_type cls params in

  Printf.sprintf "::cpp::marshal::Boxed< %s >" p, Printf.sprintf "::cpp::marshal::Boxed_obj< %s >" p

let get_extern_value_type_struct cls params =
  Printf.sprintf "::cpp::marshal::ValueType< %s >" (get_extern_value_type cls params)

let get_extern_value_type_reference cls params =
  Printf.sprintf "::cpp::marshal::Reference< %s >" (get_extern_value_type cls params)