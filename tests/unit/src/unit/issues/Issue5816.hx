package unit.issues;

import haxe.Int64;

class Issue5816 extends unit.Test {
	static var value:Int64 = 0;

	function test() {
		t(value == 0);
		value++;
		t(value == 1);
	}
}