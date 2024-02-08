package unit.issues;

import utest.Assert;

class Issue7642 extends unit.Test {
	@:analyzer(ignore)
	function test() {
		apply();
		noAssert();
	}

	@:analyzer(ignore)
	static public inline function apply(n = 0):Void {
		new Map<Int, Int>();
		//avoid infinite recursion
		if(n < 3) {
			apply(++n);
		}
	}
}