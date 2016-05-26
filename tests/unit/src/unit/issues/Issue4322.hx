package unit.issues;

private class InlineTest {
	public var a: Int;
	public var b: Int;

	@:extern
	public inline function new(a: Int, b: Int) {
		this.a = a;
		this.b = b;
	}
}

class Issue4322 extends Test {
	function test() {
		var c = new InlineTest(3, 4);
		eq(3, c.a);
		eq(4, c.b);
	}
}