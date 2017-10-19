package unit.issues;

class Issue6062 extends unit.Test {
	function test() {
		var child = Type.createInstance(Child, []);
		t(child.parentFn());
	}
}

private class Parent {
	public var parentFn = function() return true;
	function new() {}
}

private class Child extends Parent {}