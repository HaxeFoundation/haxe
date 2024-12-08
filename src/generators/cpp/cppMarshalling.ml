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

  Printf.sprintf "::cpp::Boxed< %s >" p, Printf.sprintf "::cpp::Boxed_obj< %s >" p

let get_extern_value_type_struct cls params =
  let p = get_extern_value_type cls params in
  Printf.sprintf "::cpp::Struct< %s, ::cpp::ValueTypeStructHandler< %s > >" p p 

let get_extern_value_type_reference cls params =
  Printf.sprintf "::cpp::Reference< %s >" (get_extern_value_type cls params)