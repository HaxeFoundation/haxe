package unit.issues;

class Issue4254 extends Test {
	var px:Int = Math.floor(20);

	function test() {
		eq(20, px);
	}
}