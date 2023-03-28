package unit.issues;

import haxe.Rest;
import utest.Assert;

class Issue10906 extends Test {
	#if !jvm
	function test() {
		var a:Array<Any> = new Array<Any>();
		a.push(1);
		a.push(2);
		a.push(3);
		Assert.same([1, 2, 3], a);
		eq(3, a.length);
		var r = Rest.of(a);
		eq(1, r[0]);
		eq(3, r.length);
	}
	#end
}
