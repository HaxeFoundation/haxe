package unit.issues;

class Issue3127 extends Test
{
	public function test()
	{
		new V<Int>(3);
		var v = new V<Int>(3);
		v[0] = 1;
		eq(v[0],1);
	}
}

private typedef V<T> = haxe.ds.Vector<T>;
