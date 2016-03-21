package unit.issues;

private class C {
	public function new() { }
	@:keep function toString():String return toString();
}

class Issue4962 extends Test {
	function test() {
		var int:Dynamic = Int;
		f(Std.is(new C(), int));

		f(Std.is(new C(), Int));
	}
}
