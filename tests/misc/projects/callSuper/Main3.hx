// Case 1: Parent has @:callSuper, Child overrides with super call, GrandChild overrides without super call.
// Expected: passes because Child's method doesn't have @:callSuper.
class A {
	public function new() {}
	@:callSuper public function init() {}
}

class B extends A {
	override public function init() {
		super.init();
	}
}

class C extends B {
	override public function init() {
		trace("C.init");
	}
}

class Main3 {
	static function main() {}
}
