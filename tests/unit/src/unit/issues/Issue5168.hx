package unit.issues;

class Issue5168 extends unit.Test {
	function test() {
		f(Std.isOfType("hello", Issue5168));
		f(Std.isOfType(1, String));
	}
}