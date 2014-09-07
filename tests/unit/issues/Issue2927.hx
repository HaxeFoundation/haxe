package unit.issues;
#if java
import java.Lib;
import java.NativeArray;
#elseif cs
import cs.Lib;
import cs.NativeArray;
#end

class Issue2927 extends Test {
#if (java || cs)
	public function test()
	{
		var arr = Lib.array(new NativeArray<Int>(1));
		eq(arr.length,1);
	}
#end
}

