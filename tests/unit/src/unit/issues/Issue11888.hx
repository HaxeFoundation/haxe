package unit.issues;

import haxe.Int64;

class Issue11888 extends unit.Test {
	public function test() {
		var n:Int64 = Int64.make(0,-1);
		var result:Int64 = Int64.shl(n,1);
		eq(-2, result.low);
	}
}
