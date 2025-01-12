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