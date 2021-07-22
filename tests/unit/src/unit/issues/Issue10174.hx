package unit.issues;

class Issue10174 extends Test {
	function test() {
		//should be parsed and typed without errors
		"" is unit.issues.misc.Issue10174Foo.Bar;
		noAssert();
	}
}
