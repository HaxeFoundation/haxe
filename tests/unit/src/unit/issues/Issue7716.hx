package unit.issues;

class Issue7716 extends unit.Test {
	static public var value = 999;

	function test() {
		eq(unit.issues.misc.ISSUE7716.value(), value);
	}
}