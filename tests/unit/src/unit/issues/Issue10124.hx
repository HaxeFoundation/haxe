package unit.issues;

import utest.Assert;

using unit.issues.Issue10124.Util;

private class Util {
	public static inline function add<T>(arr:Array<T>,...values:T) {
		for (v in values)
			arr.push(v);
	}

	public static inline function addFloat<T>(arr:Array<Any>,...values:Float) {
		for (v in values)
			arr.push(v);
	}
}

class Issue10124 extends Test {
	function test1() {
		var arr = [1, 3, 4];
		arr.add(5, 6, 7);
		Assert.same([1, 3, 4, 5, 6, 7], arr);
	}

	function test2() {
		var arr = [1, 3, 4];
		arr.addFloat(5, 6, 7);
		Assert.same([1, 3, 4, 5, 6, 7], arr);
	}
}
