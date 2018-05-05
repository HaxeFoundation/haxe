package unit.issues;

private enum abstract IntAbstract(Int) from Int to Int {
	public static var value = 5;

	var var1 = 1;
	var var2 = 2;
}

class Issue4103 extends Test {
	function test() {
		var value = IntAbstract.value;
		eq(5, value);
	}
}