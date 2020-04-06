package unit.issues;

private interface I {
	public function foo():I;
	public function bar(a:A):Void;
}

private class A implements I {
	public function new() {
	}

	public function foo():A {
		return this;
	}

	public function bar(a:I):Void {

	}
}

class Issue8528 extends unit.Test {
	function test() {
		var a:A = new A();

		var i:I = a;
		eq(i, i.foo());

		i.bar(a);
	}
}