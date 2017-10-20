package unit.issues;

class Issue6062 extends unit.Test {
	function test() {
		var child = Type.createInstance(Child, []);
		t(child.parentFn());
		t(Parent.tmp);
	}
}

private class Parent {
	static public var tmp:Bool = false;

	public var parentFn = function() return true;

	function new() {
		tmp = OnlyParentNewReferencesMe.field;
	}
}

private class Child extends Parent {}

private class OnlyParentNewReferencesMe {
	static public var field = true;
}