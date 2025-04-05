package unit.issues;

import unit.Test;

class Issue11381 extends Test {
	function test() {
		var a:Int = func1(1);
		var s:String = func1("foo");
		eq("foo", s);
	}

	function func1(a:Dynamic) {
		return a;
	}
}
