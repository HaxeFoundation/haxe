package unit.issues;
import unit.Test;

class Issue11468 extends Test {

	#if hl
	function test() {
		var m = new Map<Int,hl.NativeArray<Int>>();
		t(m.get(0) == null);
		var arr = new hl.NativeArray<Int>(1);
		f(arr == null);

		var b1 = new hl.Bytes(0);
		t(b1 == null);
		var b2 = new hl.Bytes(1);
		f(b2 == null);
	}
	#end
}