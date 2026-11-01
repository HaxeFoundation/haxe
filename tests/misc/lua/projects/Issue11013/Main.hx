class Main {
	static function main() {
		// Test Int32 wrapping without bit32/bit library available.
		// require, bit32, and bit are set to nil before running, so they cannot be used
		var max:haxe.Int32 = 2147483647;
		var one:haxe.Int32 = 1;
		var result:Int = max + one;
		var expected = -2147483648;
		if (result == -2147483648) {
			Sys.println("Success");
		} else {
			Sys.println('Expected $expected but got $result');
		}
	}
}
