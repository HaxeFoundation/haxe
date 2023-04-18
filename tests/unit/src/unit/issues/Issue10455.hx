package unit.issues;

class Issue10455 extends Test {
	function test() {
		var a = [0 => true ? 1 : 0];
		eq(1, a[0]);
	}
}
