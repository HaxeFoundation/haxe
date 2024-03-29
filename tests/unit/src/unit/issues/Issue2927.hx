package unit.issues;
#if jvm
import java.Lib;
import java.NativeArray;
#end

class Issue2927 extends Test {
#if jvm
	public function test()
	{
		var arr = Lib.array(new NativeArray<Int>(1));
		eq(arr.length,1);
		var arr = Lib.array(new NativeArray<Float>(1));
		eq(arr.length,1);
		var arr = Lib.array(new NativeArray<Single>(1));
		eq(arr.length,1);
		var arr = Lib.array(new NativeArray<haxe.io.Bytes>(1));
		eq(arr.length,1);
		var arr = Lib.array(new NativeArray<haxe.Int64>(1));
		eq(arr.length,1);
	}
#end
}

