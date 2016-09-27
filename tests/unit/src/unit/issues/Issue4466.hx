package unit.issues;

class Issue4466 extends Test {
	function test() {
		var x;
		function init(y) x = y;
		init(2);
		eq(2, x);
	}
}