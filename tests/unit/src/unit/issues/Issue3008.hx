package unit.issues;

class Issue3008 extends Test {
	function test() {
		var t = new unit.issues.misc.Issue3008Class("foo");
        t.call();
	}
}