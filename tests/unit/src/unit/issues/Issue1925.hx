package unit.issues;
#if jvm
import java.NativeArray;
#end

class Issue1925 extends Test {
#if jvm
	static var d2:Array<NativeArray<Int>>;
	function test() {
		var d = new NativeArray<Int>(10);
		d2 = [d];
		d[0] = 10;
		eq(d2[0][0], 10);
	}
#end
}

