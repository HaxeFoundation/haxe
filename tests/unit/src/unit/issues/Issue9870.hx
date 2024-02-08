package unit.issues;

import utest.Assert.floatEquals;

class Issue9870 extends unit.Test {
	function test() {
		var t:Null<Float> = Math.random();
		var a = t * 2.0;
		var b = t * 2;
		var c = 2 * t;
		floatEquals(a, b);
		floatEquals(a, c);
	}
}
