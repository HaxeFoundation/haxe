package unit.issues;

class Issue1497 extends Test {
	function test() {
        unit.issues.misc.Issue1497Macro.run();
        eq(1, Issue1497DefinedClass.test());
	}
}