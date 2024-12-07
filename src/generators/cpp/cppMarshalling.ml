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

let get_extern_value_type_struct cls params =
  Printf.sprintf "::cpp::Struct< %s >" (get_extern_value_type cls params)

let get_extern_value_type_reference cls params =
  Printf.sprintf "::cpp::Reference< %s >" (get_extern_value_type cls params)