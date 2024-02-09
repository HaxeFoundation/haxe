package unit.issues;

class Issue7942 extends unit.Test {
	var x = 123.e12;
	var y = 123.E12;

	function test() {
		eq(x, y);
	}
}
