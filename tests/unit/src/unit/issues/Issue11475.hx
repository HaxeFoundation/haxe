package unit.issues;

import haxe.Int64;

class Issue11475 extends Test {
	function test() {
		var i:Int64 = Int64.make(0xF0E3FF1B, 0x00000000);
		var mcr:Int64 = Int64.ushr(i, 32);
		eq(-253493477, mcr.low);
	}
}
