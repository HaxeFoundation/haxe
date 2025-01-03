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

let get_extern_value_type_boxed value_type =
  let p = get_extern_value_type value_type in

  Printf.sprintf "::cpp::marshal::Boxed< %s >" p, Printf.sprintf "::cpp::marshal::Boxed_obj< %s >" p

let get_extern_value_type_struct value_type =
  Printf.sprintf "::cpp::marshal::ValueType< %s >" (get_extern_value_type value_type)

let get_extern_value_type_reference value_type =
  Printf.sprintf "::cpp::marshal::Reference< %s >" (get_extern_value_type value_type)