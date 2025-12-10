package unit.issues;

using unit.issues.misc.Issue12415Abstract;

class Issue12415 extends Test {
	function test() {
		var value:Issue12415Abstract = null;
		eq(true, value == null);
		eq(false, value != null);
		eq(true, value.isNull());
	}
}
