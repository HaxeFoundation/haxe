package unit.issues;

interface I12268 {
	function f():Int;
}

class C12268 implements I12268 {
	public function new() {}

	function f():Int
		return 42;
}

class Issue12268 extends Test {
	public function test() {
		var c = new C12268();
		var i:I12268 = c;
		eq(i.f(), 42);
	}
}
