package unit.issues;

class Issue8068 extends Test {
	#if (hl || lua || interp || php)
	function test() {
		var f = "foo";
		var o = {charAt: f.charAt};
		eq(o.charAt(0), "f");
		eq(o.charAt(1), "o");
		eq(o.charAt(2), "o");

		// Dynamic string closure should also bind correctly
		var d:Dynamic = "bar";
		var fn = d.charAt;
		eq(fn(0), "b");
		eq(fn(1), "a");
		eq(fn(2), "r");
	}
	#end
}
