// Case: @:callSuper is transitive — without @:callSuper(false) on Child, GrandChild must also call super.
// A has @:callSuper, B overrides with super call but no @:callSuper(false), C omits super call.
// Expected: fails because the contract from A propagates through B to C.
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

class Main4 {
	static function main() {}
}
