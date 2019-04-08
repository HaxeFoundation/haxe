package unit.issues;

import utest.Assert;

class Issue7903 extends unit.Test {
	static var tmp:String;
	function test() {
		var o = {rec:(null:Dynamic)};
		o.rec = o;
		try {
			tmp = Std.string(o);
			Assert.pass();
		} catch(e:Dynamic) {
			trace(e);
			Assert.fail('Failed to stringify an object with recursive reference');
		}
	}
}