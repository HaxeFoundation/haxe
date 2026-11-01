open CppAst
open CppAstTools
open CppError

(* If we are constructing a value type of reference state, inspect the surrounding context and choose a more appropriate construction *)
let filter_determine_construction return_type cppexpr =
  let mk_cppexpr new_expr new_type =
    { cppexpr = new_expr; cpptype = new_type; cpppos = cppexpr.cpppos }
  in

  match cppexpr.cpptype, return_type, cppexpr.cppexpr with
  (* Allocating a value type directly into a pointer type is ambiguous at best and UB C++ at worse, so forbid it entirely *)
  | _, TCppStar (TCppMarshalNativeType ((ValueClass _ | ValueEnum _), _), _), CppCall ((FuncNew _), _)
  | _, TCppPointer (_, TCppMarshalNativeType ((ValueClass _ | ValueEnum _), _)), CppCall ((FuncNew _), _)
  | _, TCppRawPointer (_, TCppMarshalNativeType ((ValueClass _ | ValueEnum _), _)), CppCall ((FuncNew _), _) ->
    cpp_abort HeapAllocationOfValueType cppexpr.cpppos
  | TCppMarshalNativeType (value_type, Reference), TCppMarshalNativeType (_, Stack), CppCall ((FuncNew _), args) ->
    let stack = TCppMarshalNativeType (value_type, Stack) in
    { cppexpr with cpptype = stack; cppexpr = CppCall ((FuncNew stack), args) }
  | TCppMarshalNativeType (value_type, Reference), TCppMarshalNativeType (_, Promoted), CppCall ((FuncNew _), args) ->
    let promoted = TCppMarshalNativeType (value_type, Promoted) in
    { cppexpr with cpptype = promoted; cppexpr = CppCall ((FuncNew promoted), args) }
  (* When constructing to a reference we lack enough info to make a more precise choice *)
  (* So just allocate on the stack and wrap in a reference *)
  (* This comes up with function calls e.g. foo(new MyValueType()) *)
  (* TFun does not give us enough info to make a more precise allocation *)
  | TCppMarshalNativeType (value_type, Reference), _, CppCall ((FuncNew _), args) ->
    let stack     = TCppMarshalNativeType(value_type, Stack) in
    let reference = TCppMarshalNativeType(value_type, Reference) in
    mk_cppexpr (CppCast ({ cppexpr with cpptype = stack; cppexpr = CppCall ((FuncNew stack), args) }, reference)) reference
  | _ ->
    cppexpr

(* Handle casting to and from value type enums and scalar values *)
let rec filter_value_enum_casting return_type cppexpr =
  let mk_cppexpr new_expr new_type =
    { cppexpr = new_expr; cpptype = new_type; cpppos = cppexpr.cpppos }
  in

  match cppexpr.cpptype, return_type with
  (* Casting from from a scalar to a value type enum *)
  | TCppScalar s, (TCppMarshalNativeType ((ValueEnum abs), (Stack | Promoted))) ->
    let casted = mk_cppexpr (CppCastScalar (cppexpr, get_native_marshalled_type (ValueEnum abs))) return_type in
    mk_cppexpr (CppCall ((FuncNew return_type), [ casted ])) return_type

  | TCppScalar s, (TCppMarshalNativeType ((ValueEnum _ as e), Reference)) ->
    let promoted = filter_value_enum_casting (TCppMarshalNativeType (e, Promoted)) cppexpr in
    mk_cppexpr (CppCast (promoted, return_type)) return_type

  (* Casting going from a value type enum to a scalar *)
  | TCppMarshalNativeType ((ValueEnum _ as e), (Stack | Promoted)), TCppScalar s ->
    let reference = TCppMarshalNativeType(e, Reference) in
    let casted = mk_cppexpr (CppCast (cppexpr, reference)) reference in
    filter_value_enum_casting return_type casted
  | TCppMarshalNativeType ((ValueEnum _), Reference), TCppScalar s ->
    let dereference = mk_cppexpr (CppDereference (cppexpr)) cppexpr.cpptype in
    mk_cppexpr (CppCastScalar (dereference, s)) return_type
  | _ ->
    cppexpr

let filter_add_boxed_pointer_construction return_type cppexpr =
  let mk_cppexpr new_expr new_type =
    { cppexpr = new_expr; cpptype = new_type; cpppos = cppexpr.cpppos }
  in

  match return_type, cppexpr.cppexpr with
  (* | CppVarDecl (var, Some expr) when is_pointer_type var.tcppv_type ->
    let construct = mk_cppexpr (CppCall ((FuncNew var.tcppv_type), [ expr ])) var.tcppv_type in
    { cppexpr with cppexpr = CppVarDecl(var, Some construct) }
  | CppVarDecl (var, None) when is_pointer_type var.tcppv_type ->
    let construct = mk_cppexpr (CppCall ((FuncNew var.tcppv_type), [])) var.tcppv_type in
    { cppexpr with cppexpr = CppVarDecl(var, Some construct) } *)
  | TCppMarshalNativeType ((Pointer _), (Stack | Promoted)), CppNull ->
    mk_cppexpr (CppCall ((FuncNew return_type), [ cppexpr ])) return_type
  | TCppMarshalNativeType ((Pointer _ as value_type), Reference), CppNull ->
    let stack = TCppMarshalNativeType (value_type, Stack) in
    let ctor  = mk_cppexpr (CppCall ((FuncNew stack), [ cppexpr ])) stack in
    mk_cppexpr (CppCast (ctor, return_type)) return_type
  | _ ->
    cppexpr