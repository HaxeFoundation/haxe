package unit.issues;

class Issue6145 extends unit.Test {
	#if (!php)
	function test() {
		var r = ~/(a)/;
		r.match("a");
		exc(() -> r.matched(2));
	}
	#end
}
