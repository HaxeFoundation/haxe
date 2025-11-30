open Type
open CppTypeUtils
open CppAst
open CppAstTools
open CppError

let autocast_filter for_cppia return_type cppexpr =
  let object_expression =
    match cppexpr.cpptype with
    | TCppVariant
    | TCppDynamic
    | TCppObject -> true
    | _ -> false
  in

  let mk_cppexpr new_expr new_type =
    { cppexpr = new_expr; cpptype = new_type; cpppos = cppexpr.cpppos }
  in

  let cast_to_var_args () =
    match cpp_variant_type_of cppexpr.cpptype with
    | TCppVoidStar
    | TCppScalar _ ->
      cppexpr
    | TCppString ->
      mk_cppexpr
        (CppVar (VarInternal (cppexpr, ".", "raw_ptr()") ))
        (TCppPointer ("ConstPointer", TCppScalar "char"))
    | TCppDynamic ->
      mk_cppexpr (CppCastNative cppexpr) TCppVoidStar
    | _ ->
      let to_dynamic = mk_cppexpr (CppCast (cppexpr, TCppDynamic)) TCppDynamic in
      mk_cppexpr (CppCastNative to_dynamic) TCppVoidStar
  in

  let cast_from_object () =
    match return_type with
    | TCppUnchanged ->
      cppexpr
    | TCppInst (t, _) as inst when Meta.has Meta.StructAccess t.cl_meta ->
      let struct_type = TCppStruct inst in
      let struct_cast =
        mk_cppexpr (CppCast (cppexpr, struct_type)) struct_type
      in
      mk_cppexpr (CppCast (struct_cast, inst)) inst
    | TCppObjectArray _
    | TCppScalarArray _
    | TCppNativePointer _
    | TCppDynamicArray
    | TCppObjectPtr
    | TCppVarArg
    | TCppMarshalManagedType _
    | TCppInst _ ->
      mk_cppexpr (CppCast (cppexpr, return_type)) return_type
    | TCppObjC k ->
      mk_cppexpr (CppCastObjC (cppexpr, k)) return_type
    | TCppObjCBlock (ret, args) ->
      mk_cppexpr (CppCastObjCBlock (cppexpr, ret, args)) return_type
    | TCppScalar scalar ->
      mk_cppexpr (CppCastScalar (cppexpr, scalar)) return_type
    | TCppString ->
      mk_cppexpr (CppCastScalar (cppexpr, "::String")) return_type
    | TCppInterface _ when cppexpr.cpptype = TCppVariant ->
      mk_cppexpr (CppCastVariant cppexpr) return_type
    | TCppDynamic when cppexpr.cpptype = TCppVariant ->
      mk_cppexpr (CppCastVariant cppexpr) return_type
    | TCppStar (t, const) as ptr ->
      let ptr_type = TCppPointer ((if const then "ConstPointer" else "Pointer"), t) in
      let ptr_cast = mk_cppexpr (CppCast (cppexpr, ptr_type)) ptr_type in
      mk_cppexpr (CppCast (ptr_cast, ptr)) ptr
    (* When going from a dynamic or variant add an explicit cast so the ::cpp::marshal::Reference constructor
     * takes care of checking the dynamic type *)
    | TCppMarshalNativeType (_, _) ->
      mk_cppexpr (CppCast (cppexpr, return_type)) return_type
    | _ ->
      cppexpr
  in

  let cast_other () =
    match (cppexpr.cpptype, return_type) with
    | _, TCppUnchanged ->
      cppexpr
    (*
      Using the 'typedef hack', where we use typedef X<T> = T, allows the
      haxe compiler to use these types interchangeably. We then work
      out the correct way to convert between them when one is expected, but another provided.

      TCppFunction: these do not really interact with the haxe function type, T
      Since they are implemented with cpp::Function, conversion to/from Dynamic should happen automatically
        CallableData<T> = T;
        FunctionData<T,ABI> = T;

      TCppObjCBlock can move in and out of Dyanmic
        ObjcBlock<T> = T;

      TCppProtocol can move in and out of Dyanmic, via delegate creation
        Protocol<T /*:interface*/ > = T;

      Explicitly wrapped type - already interacts well with Dynamic and T
        Struct<T> = T;

      TCppStar, TCppStruct, TCppReference - for interacting with native code
        Star<T> = T;
        ConstStar<T> = T;
        Reference<T> = T;
        T may be an extern class, with @:structAccess - in which case
          Dynamic interaction must be handled explicitly
      These types, plus Dynamic can be used interchangeably by haxe
      Derived/inherited types may also be mixed in
    *)
    | TCppAutoCast, _
    | TCppObjC _, TCppDynamic
    | TCppObjCBlock _, TCppDynamic ->
      mk_cppexpr (CppCast (cppexpr, return_type)) return_type
    (* Infer type from right-hand-side for pointer or reference to Dynamic *)
    | TCppReference TCppDynamic, TCppReference _ ->
      cppexpr
    | TCppReference TCppDynamic, t ->
      mk_cppexpr cppexpr.cppexpr (TCppReference t)
    | TCppStar (TCppDynamic, _), TCppStar (_, _) ->
      cppexpr
    | TCppStar (TCppDynamic, const), t ->
      mk_cppexpr cppexpr.cppexpr (TCppStar (t, const))
    | TCppStar (t, const), TCppDynamic ->
      let ptr_type = TCppPointer ((if const then "ConstPointer" else "Pointer"), t) in
      let ptr_cast = mk_cppexpr (CppCast (cppexpr, ptr_type)) ptr_type in
      mk_cppexpr (CppCast (ptr_cast, TCppDynamic)) TCppDynamic
    | TCppStar (t, const), TCppReference _
    | TCppStar (t, const), TCppInst _
    | TCppStar (t, const), TCppMarshalManagedType _
    | TCppStar (t, const), TCppStruct _ ->
      mk_cppexpr (CppDereference cppexpr) return_type
    | TCppInst (t, _), TCppStar _ when is_native_class t && match cppexpr.cppexpr with | CppCall (FuncNew _, _) -> true | _ -> false ->
      mk_cppexpr (CppNewNative cppexpr) return_type
    | TCppInst _, TCppStar (p, const)
    | TCppMarshalManagedType _, TCppStar (p, const)
    | TCppStruct _, TCppStar (p, const) ->
      mk_cppexpr (CppAddressOf cppexpr) return_type
    | TCppObjectPtr, TCppObjectPtr ->
      cppexpr
    | TCppObjectPtr, _ ->
      mk_cppexpr (CppCast (cppexpr, TCppDynamic)) TCppDynamic
    | TCppProtocol _, TCppProtocol _ ->
      cppexpr
    | t, TCppProtocol protocol ->
      mk_cppexpr (CppCastProtocol (cppexpr, protocol)) return_type
    | TCppInst (t, _) as inst, TCppDynamic when Meta.has Meta.StructAccess t.cl_meta ->
      let struct_type = TCppStruct inst in
      let struct_cast = mk_cppexpr (CppCast (cppexpr, struct_type)) struct_type in
      mk_cppexpr (CppCast (struct_cast, TCppDynamic)) TCppDynamic
    | _, TCppObjectPtr ->
      mk_cppexpr (CppCast (cppexpr, TCppObjectPtr)) TCppObjectPtr
    | TCppDynamicArray, TCppScalarArray _
    | TCppDynamicArray, TCppObjectArray _
    | TCppScalarArray _, TCppDynamicArray
    | TCppObjectArray _, TCppDynamicArray when for_cppia ->
      mk_cppexpr (CppCast (cppexpr, return_type)) return_type
    | TCppScalar from, TCppScalar too when from <> too ->
      mk_cppexpr (CppCastScalar (cppexpr, too)) return_type

    (* If we are going between two pointers or value type classes which mismatch add a cast so the reference type reinterprets the pointer *)
    (* This happens in inheritance related situations *)
    | TCppMarshalNativeType ((ValueClass (fst_cls, fst_params) | Pointer (fst_cls, fst_params)), _), TCppMarshalNativeType ((ValueClass (snd_cls, snd_params) | Pointer (snd_cls, snd_params)) as dst, _) when not (fast_eq (TInst (fst_cls, [])) (TInst (snd_cls, []))) ->
      let reference = TCppMarshalNativeType(dst, Reference) in
      mk_cppexpr (CppCast (cppexpr, reference)) reference

    (* Ensure we wrap any access to the stack or promoted type in a reference object. *)
    (* TIdents are wrapped at retyping but array access and others won't be, so this will wrap them. *)
    | TCppMarshalNativeType (ValueClass (cls, _), (Stack | Reference)), other when CppMarshalling.is_stack_only_marshalling_native_value_class cls && is_object_element other ->
      cpp_abort PromotedStackOnlyValueType cppexpr.cpppos
    | TCppMarshalNativeType (value_type, Stack), (TCppPointer _)
    | TCppMarshalNativeType (value_type, Stack), (TCppRawPointer _)
    | TCppMarshalNativeType (value_type, Stack), (TCppStar _)
    | TCppMarshalNativeType (value_type, Stack), (TCppReference _)
    | TCppMarshalNativeType (_, Stack), TCppMarshalNativeType (value_type, (Promoted)) ->
      let reference = TCppMarshalNativeType(value_type, Reference) in
      mk_cppexpr (CppCast (cppexpr, reference)) reference
    | TCppMarshalNativeType (value_type, Promoted), (TCppPointer _)
    | TCppMarshalNativeType (value_type, Promoted), (TCppRawPointer _)
    | TCppMarshalNativeType (value_type, Promoted), (TCppStar _)
    | TCppMarshalNativeType (value_type, Promoted), (TCppReference _)
    | TCppMarshalNativeType (_, Promoted), TCppMarshalNativeType (value_type, (Stack)) ->
      let reference = TCppMarshalNativeType(value_type, Reference) in
      mk_cppexpr (CppCast (cppexpr, reference)) reference
    | TCppMarshalNativeType (value_type, (Stack | Promoted)), (TCppMarshalNativeType (_, Reference) | TCppDynamic | TCppVariant) ->
      let reference = TCppMarshalNativeType(value_type, Reference) in
      mk_cppexpr (CppCast (cppexpr, reference)) reference
    | _ -> cppexpr
  in

  match return_type with
  | TCppVoid ->
    mk_cppexpr cppexpr.cppexpr TCppVoid
  | TCppVarArg ->
    cast_to_var_args ()
  | _ when object_expression ->
    cast_from_object ()
  | _ ->
    cast_other ()