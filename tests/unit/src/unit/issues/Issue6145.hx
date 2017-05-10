package unit.issues;

class Issue6145 extends unit.Test {
	function test() {
		var r = ~/(a)/;
		r.match("a");
		#if (!cs && !php)
		exc(() -> r.matched(2));
		#end
	}
}