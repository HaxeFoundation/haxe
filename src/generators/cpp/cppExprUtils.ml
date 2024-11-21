open Extlib_leftovers
open Ast
open Type
open Error
open Common
open Globals

let rec remove_parens expression =
  match expression.eexpr with
  | TParenthesis e -> remove_parens e
  | TMeta(_,e) -> remove_parens e
  | _ -> expression

let rec remove_parens_cast expression =
  match expression.eexpr with
  | TParenthesis e -> remove_parens_cast e
  | TMeta(_,e) -> remove_parens_cast e
  | TCast ( e,None) -> remove_parens_cast e
  | _ -> expression

let is_static_access obj =
  match (remove_parens obj).eexpr with
  | TTypeExpr _ -> true
  | _ -> false