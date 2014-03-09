package unit.issues;
import unit.Test;

class Issue2735 extends Test {
	function test() {
        var uint:UInt = 0xFFFFFFFF;
        var f:Float = uint;
        feq(4294967295., f);
	}
}