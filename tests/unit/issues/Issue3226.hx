package unit.issues;

class Issue3226 extends Test {
	#if js
	function testJs() {
		var a = 1;
		var v = 2;
		untyped __js__("{0} = {0} + {1}", a, v);
		eq(3, a);
	}
	#elseif cpp
	function testCpp() {
		var a = 1;
		var v = 2;
		untyped __cpp__("{0} = {0} + {1}", a, v);
		eq(3, a);
	}
	#end
}