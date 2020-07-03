package unit.issues;

import haxe.ds.ReadOnlyArray;

using Lambda;

class Issue9102 extends unit.Test {
	function test() {
		var a:ReadOnlyArray<Int> = [1, 2, 3];
		t(a.exists(i -> i == 1));
	}
}
