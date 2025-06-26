package unit.issues;

class Issue10173 extends Test {
	function test() {
		var foo:Foo = Bar;
		var i = 9999;
		t(foo == i);
		foo = Baz;
		f(foo == i);
	}
}

private enum abstract Foo(Int) to Int {
	var Bar;
	var Baz;

	@:op(A == B) public static function eqInt(lhs:Foo, rhs:Int):Bool {
		return switch lhs {
			case Bar: true;
			case Baz: false;
		}
	}
}