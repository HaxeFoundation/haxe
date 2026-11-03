class Base {
	private function new() {}
	var v = 0;
}

class Child extends Base {
	private function new() super();
}

class Main2 extends Base {
	static function main() {
		// shouldn't be possible; Child's constructor is private
		new Child();
	}
}
