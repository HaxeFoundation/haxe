class Main {
	static function main() {
		var test:Dynamic = new Test();
		test.get12();
	}
}

class Test {
	public function new() { }
	public function get12() return 12;
}