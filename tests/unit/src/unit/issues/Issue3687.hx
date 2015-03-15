package unit.issues;

class Issue3687 extends Test {
	function test() {
		var a = 0;
		var b = 0;
		b = 1;
		a += b;
		eq(a, 1);
	}
}
