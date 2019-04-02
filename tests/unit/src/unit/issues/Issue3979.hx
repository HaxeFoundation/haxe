package unit.issues;
#if java
import java.NativeArray;
#elseif cs
import cs.NativeArray;
#end

class Issue3979 extends Test
{
	#if (java || cs)
	public function test()
	{
		var v = 0;
		var nv = NativeArray.make(1,2,3,4,5,6);
		eq(nv.length,6);
		for (val in nv)
		{
			eq(val,++v);
		}
		eq(v,6);
	}
	#end
}
