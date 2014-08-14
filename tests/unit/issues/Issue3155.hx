package unit.issues;

@:generic
private class A<T> {
	private function f() {
		return 12;
	}
}

private class B extends A<Int> {
	public function new() { }
	override public function f() {
		return super.f();
	}
}

class Issue3155 extends Test {
	function test() {
		var a = new B();
		eq(12, a.f());
	}
}