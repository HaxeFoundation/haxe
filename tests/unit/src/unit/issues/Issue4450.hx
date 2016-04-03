package unit.issues;

class Issue4450 extends Test {
	function test() {
		@meta var some = 1;
		eq(1, some);
	}
}