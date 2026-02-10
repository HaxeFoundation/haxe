class Main {
	static function main() {
		// Test Int32 wrapping without bit32 library available.
		// require is set to nil before running, so bit32 cannot be loaded.
		var max:haxe.Int32 = 2147483647;
		var one:haxe.Int32 = 1;
		var result:Int = max + one;
		if (result == -2147483648) {
			Sys.stderr().writeString("Success");
			Sys.stderr().flush();
		} else {
			Sys.stderr().writeString('Expected -2147483648 but got $result');
			Sys.stderr().flush();
		}
	}
}
