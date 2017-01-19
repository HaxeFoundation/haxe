package unit.issues;
import unit.Test;

class Issue2844 extends Test {
	function test() {
		t(unit.HelperMacros.typeError({
			var a:Array<Int> = [];
			var b:Array<Float> = a;
		}));
	}
}