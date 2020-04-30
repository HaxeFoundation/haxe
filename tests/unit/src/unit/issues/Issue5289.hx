package unit.issues;

class Issue5289 extends Test {
	static var minInt32:Int = -2147483648;

	function test() {
		eq(-2147483648, minInt32);
	}
}