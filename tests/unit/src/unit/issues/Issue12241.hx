package unit.issues;

private interface I {
	function f<T, C:Array<T>>(c:C):C;
}

private class C implements I {
	public function new() {}

	public function f<T, C:Array<T>>(c:C) {
		return c;
	}
}

class Issue12241 extends Test {
	function test() {
		var c:I = new C();
		var a = [];
		eq(a, c.f(a));
	}
}
