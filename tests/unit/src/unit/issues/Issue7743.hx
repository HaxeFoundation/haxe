package unit.issues;

class Issue7743 extends unit.Test {
	function test() {
		for (i in 0...0) {}
		noAssert();
	}
}