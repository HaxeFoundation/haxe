package unit.issues;

class Issue6942 extends unit.Test {
	function test() {
		eq(1, -First);
		eq(2, 1 - First);
	}
}

private enum abstract Test(Int) from Int to Int {
	var First = -1;
}
