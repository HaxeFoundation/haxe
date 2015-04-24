package unit.issues;

import haxe.Int64;
abstract FunInt64(Int64)
{
    public static var NEGATIVE_ONE(default, null) : FunInt64 = FunInt64.make(0xffffffff, 0xffffffff);
    public static var ZERO(default, null) : FunInt64 = FunInt64.make(0x00000000, 0x00000000);

    public static inline function make(high : Int, low : Int) : FunInt64
    {
        return new FunInt64(Int64.make(high, low));
    }

    @:op(A>>B) public static inline function shrs_i(j : FunInt64, k : Int)
    {
        if (k >= 63)
        {
            return ((j : Int64).high < 0) ? NEGATIVE_ONE : ZERO;
        }
        return new FunInt64(Int64.shr(j, k));
    }

    public inline function new(a : Int64) : Int64
    {
        this = a;
    }

    @:to public inline function toInt64()
    {
        return this;
    }
}

class Issue3370 extends unit.Test {
	function test() {
		var val64 : FunInt64 = FunInt64.make(0, 2);
		val64 = val64 >> 1;
		eq(1, Int64.toInt(val64.toInt64()));
	}
}