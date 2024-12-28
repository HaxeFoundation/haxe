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

let filter_assign_local_type tcppexpr =
  match tcppexpr.cppexpr with
  | CppVarDecl ({ tcppv_type = TCppValueType (cls, params, Reference) } as var, init) ->
    let new_type = if has_var_flag var.tcppv_var VCaptured then Promoted else Stack in
    { tcppexpr with cppexpr = CppVarDecl ({ var with tcppv_type = TCppValueType (cls, params, new_type) }, init) }
  | _ ->
    tcppexpr
