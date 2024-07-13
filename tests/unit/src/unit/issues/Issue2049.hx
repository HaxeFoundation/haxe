package unit.issues;
#if jvm
import java.NativeArray;
import java.Lib;
#end

class Issue2049 extends unit.Test
{
#if jvm
	public function test()
	{
		var arr = [ 1., 1., 1., 0.5 ].map( function( n: Float ): Single { return n; });
		var scaleFactors:NativeArray<Single> = Lib.nativeArray( arr, true );
		eq(1.,scaleFactors[0]);
		eq(1.,scaleFactors[1]);
		eq(1.,scaleFactors[2]);
		eq(.5,scaleFactors[3]);
	}
#end
}
