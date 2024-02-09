package unit.issues;

private class A {
	public var a:String;
	public inline function new(a:String) {
		this.a = a;
	}

	public function test() { return a; }
}

class Issue3967 extends Test {
	function testAndy() {
		var a = ["a", "b"];
		var b = a.copy();
		eq("a", b[0]);
		eq("b", b[1]);
		f(a == b);
	}

	function testNicolas() {
		var x:{ var test(default,never) : () -> String; } = new A("foo");
		eq("foo", x.test());
	}
}