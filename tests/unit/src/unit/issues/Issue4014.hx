package unit.issues;

import haxe.Int64;

class Issue4014 extends Test
{
	public function test()
	{
		var d = Int64.make(1,1);
		var dyn:Dynamic = d;
		t(Int64.isInt64(dyn));
		d = dyn;
		eq(d.high,1);
		eq(d.low,1);
		dyn = {};
		f(Int64.isInt64(dyn));
	}
}
