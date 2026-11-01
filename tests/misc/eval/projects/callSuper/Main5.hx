// Case 3: Parent has @:callSuper, Child does NOT override, GrandChild overrides without super call.
// Expected: fails because GrandChild directly overrides A.init which has @:callSuper.
class A {
	public function new() {}
	@:callSuper public function init() {}
}

class B extends A {
	// No override of init here
}

class C extends B {
	override public function init() {
		trace("C.init");
	}
}

class Main5 {
	static function main() {}
}
