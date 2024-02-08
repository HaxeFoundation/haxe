package unit.issues;

using unit.issues.Issue10144;

class Issue10144 extends Test {
	static final foo = (a:Int) -> a * 2;

	function test() {
		eq(10, 5.foo());
	}
}