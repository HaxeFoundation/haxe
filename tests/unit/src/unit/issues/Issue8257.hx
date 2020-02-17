package unit.issues;

import utest.Assert;
private abstract Dyn(Dynamic) from Dynamic {
	@:to function toDynamic():Dynamic return this;
}

class Issue8257 extends unit.Test {
	function test() {
		var value:Dyn = "s";
		(try value catch (pokemon:Dynamic) null : String);
		Assert.pass();
	}
}