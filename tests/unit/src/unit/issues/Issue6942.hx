package unit.issues;

class Issue6942 extends unit.Test {
	static inline var FLOAT_INLINE = -1.0;
	static inline var INT_INLINE = -1;

	function test() {
		eq(1, -IntEnum);
		eq(2, 1 - IntEnum);

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

enum abstract IntTest(UInt) from UInt to UInt {
 var IntEnum = -1;
}