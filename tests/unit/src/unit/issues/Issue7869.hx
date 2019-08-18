package unit.issues;

private class A {
	function new() {}
}

private class B extends A {
	public var f:Int;
	public var x:()->Void;

	public function new() {
		super();
		x = () -> f = 2;
	}
}

class Issue7869 extends unit.Test {
	function test() {
		var b = new B();
		b.x();
		eq(b.f, 2);
	}
}
