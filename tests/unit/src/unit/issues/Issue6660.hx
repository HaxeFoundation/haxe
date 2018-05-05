package unit.issues;

private class BaseClass {
	public function new() { }

	dynamic public function test() {
		return "test";
	}
}

private class ChildClass extends BaseClass {
	public function new() {
		super();
	}
}

private class GrandChildClass extends ChildClass {
	public function new() {
		super();
	}
}

class Issue6660 extends unit.Test {
	function test() {
		var child = new ChildClass();
		eq("test", child.test());

		var child = new GrandChildClass();
		eq("test", child.test());
	}
}