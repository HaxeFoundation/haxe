package unit.issues;

using unit.issues.misc.Issue3679Abstract;

class Issue3679 extends Test {
	function test() {
		var a = new Issue3679Abstract(12);
		eq(12, a.extract());
	}
}