package unit.issues;

class Issue5303 extends unit.Test {
	function test() {
		var a:Int = 0;
		a = -2147483648 - a;
		// we only care if this type-checks
		noAssert();
	}
}