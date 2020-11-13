package unit.issues;

class Issue9904 extends unit.Test {
	function test() {
		f(Std.parseFloat("a") == null);
		t(Math.isNaN(Std.parseFloat("a")));
	}
}
