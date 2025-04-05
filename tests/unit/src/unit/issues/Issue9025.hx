package unit.issues;

class Issue9025 extends unit.Test {
	function test() {
		var a:A = Type.createInstance(A, [true]);
		t(a.field);
	}
}

@:keep
private class A {
	public var field:Bool = false;
	public function new(b:Bool) {
		field = b;
	}
}