package unit.issues;

import haxe.ds.List;

private abstract MyOpaciteInt(Int) {}

class Issue3110 extends Test {
	function test() {
		var o:List<MyOpaciteInt> = null;
		t(unit.HelperMacros.typeError({
			var u:List<UInt> = o;
		}));
	}
}