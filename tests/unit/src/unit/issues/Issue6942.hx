package unit.issues;

class Issue6942 extends unit.Test {
	static inline var FLOAT_INLINE = -1.0;
	static inline var INT_INLINE = -1;

	function test() {
		eq(1, -IntEnum);
		eq(2, 1 - IntEnum);

		// With UInt32, negation via Int cast (two's complement)
		eq(1, -(cast UIntEnum : Int));
		eq(2, 1 - (cast UIntEnum : Int));

		eq(1, -INT_INLINE);
		eq(2, 1 - INT_INLINE);

		// TODO: fix https://github.com/HaxeFoundation/haxe/issues/8321
		// eq(1.0, -FloatEnum);
		// eq(2.0, 1 - FloatEnum);

		eq(1.0, -FLOAT_INLINE);
		eq(2.0, 1 - FLOAT_INLINE);
	}
}

enum abstract FloatTest(Float) from Float to Float {
	var FloatEnum = -1.0;
}

enum abstract IntTest(Int) from Int to Int {
	var IntEnum = -1;
}

enum abstract UIntTest(haxe.UInt32) from haxe.UInt32 to haxe.UInt32 {
	var UIntEnum = cast 0xFFFFFFFF; // represents -1 in two's complement
}
