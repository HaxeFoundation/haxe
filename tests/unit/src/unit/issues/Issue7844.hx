package unit.issues;

private class A {
	@:keep
	@:native("other")
	function some() {
		return "A";
	}
}

private class B extends A {}

private class C extends B {
	public function new() {}

	override public function some() {
		return super.some() + "C";
	}
}

class Issue7844 extends unit.Test {
	function test() {
		var c = new C();
		var f = Reflect.field(c, "other");
		var r = Reflect.callMethod(c, f, []);
		eq(r, "AC");
	}
}