package unit.issues;

class Issue9144 extends Test {
	function test() {
		var i = 1;
		var x:Map<Int, Int> = [i => (i = 2), i => 3];
		eq(2, x[1]);
		eq(3, x[2]);
	}
}