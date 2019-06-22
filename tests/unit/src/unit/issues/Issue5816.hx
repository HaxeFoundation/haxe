package unit.issues;

import haxe.Int64;

class Issue5816 extends unit.Test {
	static var value:Int64 = 0;

	function test() {
		eq((0:Int64), value);
		value++;
		eq((1:Int64), value);
	}
}