package unit.issues;

import unit.HelperMacros.typeString;

class Issue10744 extends Test {
	function test() {
		var v:Null<Int> = 10;
		eq("Int", typeString(v ?? return));
		eq("Int", typeString(v ?? throw true));
		for (i in 0...1) {
			eq("Int", typeString(v ?? break));
			eq("Int", typeString(v ?? continue));
		}
		eq("Int", typeString(v ?? {
			(throw "nope");
		}));
		eq("Null<Int>", typeString(v ?? {
			if (Std.random(0) == 0)
				return;
			else
				v;
		}));
	}
}
