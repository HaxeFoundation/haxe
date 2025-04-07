package unit.issues;

import utest.Assert;

class Issue12031 extends Test {
	#if hl
	inline function inlinef(v:hl.I64) {
		var str = "";
		str += (v >> 32);
		return str;
	}

	function noInlinef(v:hl.I64) {
		var str = "";
		str += (v >> 32);
		return str;
	}

	function test() {
		var base:hl.I64 = 22; // 0b10110
		eq(22, base.toInt());
		eq("0", inlinef(base));
		eq("0", noInlinef(base));
		var manual1 = "" + (base >> 33);
		eq("0", manual1);
		var manual2 = "" + (base >> 34);
		eq("0", manual2);
	}
	#end
}
