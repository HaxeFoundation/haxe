package unit.issues;

class Issue9715 extends unit.Test {
#if php
	function test() {
		var o = {hello:'world'};
		var s = php.Global.serialize(o);
		utest.Assert.same(o, php.Global.unserialize(s));
	}
#end
}
