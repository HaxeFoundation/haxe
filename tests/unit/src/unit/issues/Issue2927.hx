package unit.issues;

#if jvm
import jvm.NativeArray;
#end

class Issue2927 extends Test {
	#if jvm
	public function test() {
		var arr = new NativeArray<Int>(1).toArray();
		eq(arr.length, 1);
		var arr = new NativeArray<Float>(1).toArray();
		eq(arr.length, 1);
		var arr = new NativeArray<Single>(1).toArray();
		eq(arr.length, 1);
		var arr = new NativeArray<haxe.io.Bytes>(1).toArray();
		eq(arr.length, 1);
		var arr = new NativeArray<haxe.Int64>(1).toArray();
		eq(arr.length, 1);
	}
	#end
}
