package unit.issues;
import unit.Test;

class Issue2735 extends Test {
	function test() {
		#if loose_numeric_casts
		var uint:UInt = 0xFFFFFFFF;
		var f:Float = uint;
		feq(4294967295., f);
		#else
		noAssert();
		#end
	}
}