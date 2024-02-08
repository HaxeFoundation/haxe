package unit.issues;

import utest.Assert;

class Issue8220 extends unit.Test {
	@:analyzer(ignore)
	function test() {
		var height = 10;
		breaker(height);
	}

	@:analyzer(ignore)
	inline function breaker(height:Float):Void {
		eq(-0.1, 1 / -height);
	}
}