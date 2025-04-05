package unit.issues;

import utest.Assert;

@:forward
private abstract Suite(SuiteObject) from SuiteObject to SuiteObject {
	@:from
	public static macro function ofAny(expr:haxe.macro.Expr) {
		return expr;
	}
}


private interface SuiteObject {}

private class BasicSuite implements SuiteObject {
	public function new() {}
}

class Issue8779 extends unit.Test {
	function test() {
		var suite:Suite = new BasicSuite();
		Assert.pass();
	}
}