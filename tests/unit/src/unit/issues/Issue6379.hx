package unit.issues;

class Issue6379 extends unit.Test {
	#if (!java && !lua)
	function test() {
        eq(g("x_x").length, 2);
    }

	function g(s) {
		var r = s.split("_");
		g.bind("");
        return r;
	}
	#end
}