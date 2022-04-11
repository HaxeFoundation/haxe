package unit.issues;

using unit.issues.Issue10445;

class Issue10445 extends Test {
	function test() {
		env(null).provide(1);
		noAssert();
	}

	function env<TR1:TR0,TR0>(a:TR0):Dummy<TR1>
		return null;

	static function provide <TR2>(a:Dummy<TR2>, r:TR2) {}
}

private enum Dummy<T> {}