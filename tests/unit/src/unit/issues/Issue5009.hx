package unit.issues;

import unit.issues.misc.Issue5009Assert.ASSert;

class Issue5009 extends unit.Test {
	function test() {
		ASSert(true);
		noAssert();
	}
}