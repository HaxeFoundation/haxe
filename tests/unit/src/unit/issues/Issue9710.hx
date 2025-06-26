package unit.issues;

import haxe.ds.ReadOnlyArray;

class Issue9710 extends unit.Test {
	function test() {
		var a:ReadOnlyArray<Int> = [1];
		var b:ReadOnlyArray<Int> = [2];
		aeq([1, 2], a.concat(b));
	}
}
