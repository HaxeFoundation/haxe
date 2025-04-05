class Test {
	@:deprecated
	public static macro function test() {
		return macro {};
	}
}

class Main {
	public static function main() {
		Test.test();
	}
}