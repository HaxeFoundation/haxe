package unit.issues;

class Issue8068 extends Test {
	function test() {
		#if lua
		var f = "foo";
		var o = {charAt: f.charAt};
		eq(o.charAt(0), "f");
		eq(o.charAt(1), "o");
		eq(o.charAt(2), "o");
		#else
		noAssert();
		#end
	}
}
