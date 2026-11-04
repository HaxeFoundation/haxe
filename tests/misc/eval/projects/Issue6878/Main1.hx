class Main {
    public static function main() {
		test("10");
		test(10);
    }

	@:generic static function test<T:String>(t:T) { }
}