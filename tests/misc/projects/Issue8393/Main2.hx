class Main2 {

	@:generic
	static function test<T>(i:T) {}

	static function test_Int(i:Int) {}

	static function main():Void {
		test(10);
	}
}