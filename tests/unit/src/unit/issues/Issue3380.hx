package unit.issues;

class Issue3380 extends Test {
	function test() {
		var a = 0;
		a = a++;
		eq(0, a);
	}
}