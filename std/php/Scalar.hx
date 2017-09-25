package php;

import haxe.extern.EitherType;


/**
    `Scalar` is a type that is compatible with any scalar value (int, float, bool, string)
**/
typedef Scalar = EitherType<Int, EitherType<String, EitherType<Float, Bool>>>;