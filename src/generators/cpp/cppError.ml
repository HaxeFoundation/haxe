open Error

type cppError =
    | InternalError
    | MissingValueSemantics
    | NativeMarshallingFunctionClosures
    | InvalidMarshallingTypeParameter
    | PointerTypeConstructor
    | ValueTypeUndefined
    | InvalidNamespaceField
    | InvalidTypeField
    | InvalidFlagsField
    | ExtendingManagedType
    | UnresolvedTypeParameter of string
    | PromotedStackOnlyValueType
    | HeapAllocationOfValueType
    | InvalidIntrinsicUse

let cpp_abort error pos =
    let print = Printf.sprintf "CPP%0*i: %s" 4 in
    let str =
        match error with
        | InternalError -> print 0 "Internal compiler error"
        | MissingValueSemantics -> print 1 "Marshalling type extern must be annotated with value semantics"
        | NativeMarshallingFunctionClosures -> print 2 "Native marshalling types cannot have function closures created for them"
        | InvalidMarshallingTypeParameter -> print 3 "Invalid parameter for a marshalling type"
        | PointerTypeConstructor -> print 4 "Pointer type cannot have a constructor"
        | ValueTypeUndefined -> print 5 "Marshalling value type extern cannot be used for a variable declaration with no expression"
        | InvalidNamespaceField -> print 6 "Namespace field must be an array declaration of string literals"
        | InvalidTypeField -> print 7 "Type field must be a string literal"
        | InvalidFlagsField -> print 8 "Flags field must be an array of identifiers"
        | ExtendingManagedType -> print 9 "Class cannot extend a managed type extern"
        | UnresolvedTypeParameter s -> print 10 (Printf.sprintf "Unable to resolve parameter %s, consider adding a type hint" s)
        | PromotedStackOnlyValueType -> print 11 "Marshalling value type with the StackOnly flag cannot be promoted to the heap"
        | HeapAllocationOfValueType -> print 12 "Value type cannot be allocated directly to a pointer"
        | InvalidIntrinsicUse -> print 13 "Intrinsic functions can only be directly called"
    in

    abort str pos