open Ast
open Type
open Error
open Globals
open CppTypeUtils
open CppAst
open CppAstTools
open CppContext

let filter_class_field_access tcppexpr =
  let mk_cppexpr new_expr new_type =
    { cppexpr = new_expr; cpptype = new_type; cpppos = tcppexpr.cpppos }
  in

  match tcppexpr.cpptype, tcppexpr.cppexpr with
  | TCppValueType _ as vt, CppVar (VarInstance (retyped_obj, member, cls, operator)) ->
    mk_cppexpr (CppCast (tcppexpr, vt)) vt
  | _, _ ->
    tcppexpr