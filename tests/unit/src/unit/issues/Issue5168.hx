package unit.issues;

class Issue5168 extends unit.Test {
	function test() {
		f(Std.is("hello", Issue5168));
		f(Std.is(1, String));
	}
}