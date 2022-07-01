package unit.issues;

import unit.HelperMacros.typeString;

class Issue10744 extends Test {
	function test() {
		var v:Null<Int> = 10;
		eq("Null<Int>", typeString(v ?? return));
		for (i in 0...1) {
			eq("Null<Int>", typeString(v ?? break));
			eq("Null<Int>", typeString(v ?? continue));
		}
	}
}
