package unit.issues;

private class C {
	public function new() { }
	@:keep function toString():String return toString();
}

class Issue4962 extends Test {
	function test() {
		var int:Dynamic = Int;
		f(Std.isOfType(new C(), int));

		f(Std.isOfType(new C(), Int));
	}
}
