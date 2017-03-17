package unit.issues;

private class A {
	public var x : Int = 1;
	public inline function new () {};
}

private class B extends A {
	public var y : Int = 2;
	public inline function new () { super(); };
}

class Issue6093 extends unit.Test {
	function test() {
		var b = new B();
		eq(b.x, 1);
		eq(b.y, 2);
	}
}