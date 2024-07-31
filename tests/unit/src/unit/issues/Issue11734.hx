package unit.issues;

import unit.Test;
#if hl
import hl.NativeArray;
#end

class Issue11734 extends Test {
	#if hl
	function test() {
		var a = new hl.NativeArray<Float>(1);
		a[0] = 0.0;
		var b:NativeArray<Float> = new hl.NativeArray<Float>(1);
		b[0] = 1.0;
		a.blit(0, b, 0, 1);
		feq(1.0, a[0]);
	}
	#end
}
