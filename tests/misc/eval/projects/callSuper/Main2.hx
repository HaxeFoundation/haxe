class A {
	public function new() {}

	@:callSuper public function init() {}
}

class B extends A {
	override public function init() {
		super.init();
	}
}

class C extends A {
	var done = false;

	override public function init() {
		if (done) return;
		done = true;
		super.init();
	}
}

class Main2 {
	static function main() {}
}
