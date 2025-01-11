open Ast
open Type
open Error
open Globals
open CppTypeUtils
open CppAst
open CppAstTools
open CppContext

(* If we are constructing a value type of reference state, inspect the surrounding context and choose a more appropriate construction *)
let filter_determine_construction return_type cppexpr =
  let mk_cppexpr new_expr new_type =
    { cppexpr = new_expr; cpptype = new_type; cpppos = cppexpr.cpppos }
  in

  match cppexpr.cpptype, return_type, cppexpr.cppexpr with
  (* When constructing to a reference we lack enough info to make a more precise choice *)
  (* So just allocate on the stack and wrap in a reference *)
  (* This comes up with function calls e.g. foo(new MyValueType()) *)
  (* TFun does not give us enough info to make a more precise allocation *)
  | TCppMarshalType (value_type, Reference), TCppMarshalType (_, Reference), CppCall ((FuncNew _), args) ->
    let stack     = TCppMarshalType(value_type, Stack) in
    let reference = TCppMarshalType(value_type, Reference) in
    mk_cppexpr (CppCast ({ cppexpr with cpptype = stack; cppexpr = CppCall ((FuncNew stack), args) }, reference)) reference
  | TCppMarshalType (value_type, Reference), TCppMarshalType (_, Stack), CppCall ((FuncNew _), args) ->
    let stack = TCppMarshalType (value_type, Stack) in
    { cppexpr with cpptype = stack; cppexpr = CppCall ((FuncNew stack), args) }
  | TCppMarshalType (value_type, Reference), _, CppCall ((FuncNew _), args) ->
    let promoted = TCppMarshalType (value_type, Promoted) in
    { cppexpr with cpptype = promoted; cppexpr = CppCall ((FuncNew promoted), args) }
  | _ ->
    cppexpr

(* Handle casting to and from value type enums and scalar values *)
let rec filter_value_enum_casting return_type cppexpr =
  let mk_cppexpr new_expr new_type =
    { cppexpr = new_expr; cpptype = new_type; cpppos = cppexpr.cpppos }
  in

  match cppexpr.cpptype, return_type with
  (* Casting from from a scalar to a value type enum *)
  | TCppScalar s, (TCppMarshalType ((ValueEnum (abs, _)), (Stack | Promoted))) ->
    let casted = mk_cppexpr (CppCastScalar (cppexpr, get_extern_enum_value_type abs)) return_type in
    mk_cppexpr (CppCall ((FuncNew return_type), [ casted ])) return_type

  | TCppScalar s, (TCppMarshalType ((ValueEnum _ as e), Reference)) ->
    let promoted = filter_value_enum_casting (TCppMarshalType (e, Promoted)) cppexpr in
    mk_cppexpr (CppCast (promoted, return_type)) return_type

  (* Casting going from a value type enum to a scalar *)
  | TCppMarshalType ((ValueEnum _ as e), (Stack | Promoted)), TCppScalar s ->
    let reference = TCppMarshalType(e, Reference) in
    let casted = mk_cppexpr (CppCast (cppexpr, reference)) reference in
    filter_value_enum_casting return_type casted
  | TCppMarshalType ((ValueEnum _), Reference), TCppScalar s ->
    let dereference = mk_cppexpr (CppDereference (cppexpr)) cppexpr.cpptype in
    mk_cppexpr (CppCastScalar (dereference, s)) return_type
  | _ ->
    cppexpr

(* If the RHS of an set operation is a value type ensure its a reference *)
(* This allows the reference auto cast operators to handle lots of stuff for us *)
let filter_value_type_assignment return_type cppexpr =
  let mk_cppexpr new_expr new_type =
    { cppexpr = new_expr; cpptype = new_type; cpppos = cppexpr.cpppos }
  in

  match cppexpr.cppexpr with
  | CppSet (l, ({ cpptype = TCppMarshalType (vt, (Stack | Promoted)) } as r)) ->
    let reference = TCppMarshalType (vt, Reference) in
    let cast      = mk_cppexpr (CppCast (r, reference)) reference in
    { cppexpr with cppexpr = CppSet(l, cast) }
  | _ ->
    cppexpr

let filter_add_boxed_pointer_construction return_type cppexpr =
  let mk_cppexpr new_expr new_type =
    { cppexpr = new_expr; cpptype = new_type; cpppos = cppexpr.cpppos }
  in

  let is_pointer_type tcpp =
    match tcpp with
    | TCppMarshalType ((Pointer _), _) ->
      true
    | _ ->
      false
    in

  match cppexpr.cppexpr with
  (* | CppVarDecl (var, Some expr) when is_pointer_type var.tcppv_type ->
    let construct = mk_cppexpr (CppCall ((FuncNew var.tcppv_type), [ expr ])) var.tcppv_type in
    { cppexpr with cppexpr = CppVarDecl(var, Some construct) }
  | CppVarDecl (var, None) when is_pointer_type var.tcppv_type ->
    let construct = mk_cppexpr (CppCall ((FuncNew var.tcppv_type), [])) var.tcppv_type in
    { cppexpr with cppexpr = CppVarDecl(var, Some construct) } *)
  | CppNull when is_pointer_type return_type ->
    mk_cppexpr (CppCall ((FuncNew return_type), [ cppexpr ])) return_type
  | _ ->
    cppexpr