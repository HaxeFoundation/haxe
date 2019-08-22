package unit.issues;

class Issue4639 extends unit.Test {
	static var Issue4639:Int = 10;
	function test() {
		eq(10, Issue4639);
		eq(A, A);
	}
}

private enum A { A; }