package unit.issues;

import utest.Assert;

class Issue10405 extends Test {
	#if jvm
	public overload function onEvent(v:Int):Void {}

	public overload function onEvent() {
		var b = Math.random() > 0.5 ? 1 : throw "no";
	}

	function test() {
		Assert.pass();
	}
	#end
}
