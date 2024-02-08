package unit.issues;

@:generic
private abstract class A<T> {
	var i:T;

	public function new(i:T) {
		this.i = i;
	}

	abstract function foo():T;
}

private class B extends A<Int> {
	public function foo() {
		return i;
	}
}

class Issue10735 extends Test {
	function test() {
		eq(12, new B(12).foo());
	}
}
