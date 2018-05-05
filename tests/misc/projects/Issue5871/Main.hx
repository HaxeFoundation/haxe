class Base {
	var v = 0;
}

class Child extends Base {
	private function new() {}
}

class Main extends Base {
	static function main() {
		// shouldn't be possible; Child's constructor is private
		new Child();
	}
}
