// Case: @:callSuper(false) on Child breaks the chain.
// B must call super (A has @:callSuper), but C is exempt because B uses @:callSuper(false).
class A {
	public function new() {}
	@:callSuper public function init() {}
}

class B extends A {
	@:callSuper(false) override public function init() {
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
