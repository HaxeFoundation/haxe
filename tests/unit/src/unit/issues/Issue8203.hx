package unit.issues;

import utest.Assert;

class Issue8203 extends unit.Test {
	function test() {
		var x:Dummy = [1,true,"hi"];
		noAssert();
	}
}

private abstract Dummy(Dynamic)
	from Dynamic
	from Array<Dynamic>
	from Array<Int>
{}