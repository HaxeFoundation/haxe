package haxe;

/**
    An abstract type allowing values to be either of `T1` or `T2` type.
    Supports implicit casts from/to either types.

    It is useful for interfacing with external code on dynamic platforms
    such as JavaScript or Python.

    Otherwise, use of this type is discouraged.
**/
abstract EitherType<T1,T2>(Dynamic) from T1 to T1 from T2 to T2 {}
