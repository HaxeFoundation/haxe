package unit.issues;

class Issue6145 extends unit.Test {
	function test() {
		var r = ~/(a)/;
		r.match("a");
		exc(() -> r.matched(2));
	}
}