package unit.issues;

class Issue4571 extends Test {
	function test() {
		var x = -1072787802;
		eq(x, (x >>> 0));
	}
}