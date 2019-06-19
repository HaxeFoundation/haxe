package unit.issues;

import utest.Assert;

class Issue8032 extends unit.Test {
	function test() {
		// crude but effective way to test this
		#if sys
			#if target.sys
			Assert.pass();
			#else
			Assert.fail();
			#end
		#else
			#if target.sys
			Assert.fail();
			#else
			Assert.pass();
			#end
		#end
	}
}