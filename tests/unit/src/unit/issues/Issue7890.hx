package unit.issues;

class Issue7890 extends unit.Test {
	function test() {
		var x: () -> Void = () -> { return; }
		noAssert();
	}
}