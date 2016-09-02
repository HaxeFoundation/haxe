package unit.issues;

class Issue5260 extends unit.Test {
	function test() {
		inline function foo( ?value : Dynamic = true ) return value;
		t(foo());
	}
}