package unit.issues;

private abstract A(String) {
	public inline function new(s) this = s;

	@:op(A!) function opNot() {
		return this.toUpperCase();
	}
}

class Issue4284 extends Test {
	function test() {
		var a = new A("foo");
		eq("FOO", a!);

		var b = true;
		t(unit.HelperMacros.typeError(b!));
	}
}