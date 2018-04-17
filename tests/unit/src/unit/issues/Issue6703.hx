package unit.issues;
import unit.Test;

private interface A {
	function foo() : Float;
}

class Issue6703 extends Test implements A {

	var v : Float;

	public function foo() : Float {
		return v;
	}

	static function getClosure( inst : A ) {
		return inst.foo;
	}
	
	function test() {
		v = 123.45;
		eq( getClosure(this)(), 123.45 );
	}
	
}