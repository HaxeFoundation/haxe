class Base {
	public function new() { }

	public function test() { }
}

class Child extends Base {
	override function test() { }
}

class Main {
	static function main() {
		var m = new Base();
		inline m.test();
	}
}