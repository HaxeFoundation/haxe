package unit.issues;

class Issue7953 extends unit.Test {
	function test() {
		check(new Child());
	}

	@:nullSafety
	@:pure(false)
	function check(?c:Parent) {
		t(Std.is(c, Child));
	}
}

private class Parent {
	public function new() {}
}

private class Child extends Parent {}
