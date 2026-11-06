class Main {
	static function assertEquals(expected, actual) {
		if (actual == expected) {
			Sys.println("Success");
		} else {
			Sys.println('Expected $expected but got $actual');
		}
	}

	static function main() {
		// Test Int32 wrapping without bit32/bit library available.
		// require, bit32, and bit are set to nil before running, so they cannot be used
		final max:haxe.Int32 = 2147483647;
		final min:haxe.Int32 = -2147483648;
		final one:haxe.Int32 = 1;
		assertEquals(min, max + one);
		assertEquals(max, min - one);
	}
}
