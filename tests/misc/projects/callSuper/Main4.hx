// Case 2: Parent has @:callSuper, Child has @:callSuper and overrides with super call,
// GrandChild overrides without super call.
// Expected: fails because Child's method has @:callSuper.
class A {
	public function new() {}
	@:callSuper public function init() {}
}

class B extends A {
	@:callSuper override public function init() {
		super.init();
	}
}

class C extends B {
	override public function init() {
		trace("C.init");
	}
}

class Main4 {
	static function main() {}
}
