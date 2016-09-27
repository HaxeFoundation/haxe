package unit.issues;

private class Parent {
	public var x:Int;

	public inline function new(x:Int) {
		this.x = x;
	}
}

private class Child extends Parent { }

class Issue4690 extends Test {
	function test() {
		var x = new Child(12);
		eq(12, x.x);
	}
}