package unit.issues;

class Issue9904 extends unit.Test {
	function test() {
		t(Math.isNaN(Std.parseFloat("a")));
	}
}
