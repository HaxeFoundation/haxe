class Main {
	static function main() {
		test();
		#if !macro
		$type(42);
		#end
	}

	static macro function test() {
		1 = 0;
		$type(42);
		return macro {};
	}
}
