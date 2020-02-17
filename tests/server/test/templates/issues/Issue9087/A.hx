class A {
	public function new() {
		in{-1-}it(); // "go to implementation" from the call site currently yields nothing
	}

	function init() {}
}

class B extends A {
	override function {-2-}init{-3-}() {
		super.init();
	}
}
