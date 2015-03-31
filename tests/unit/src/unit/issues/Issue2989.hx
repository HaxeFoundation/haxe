package unit.issues;

class Issue2989 extends Test
{
	public function test()
	{
		var n = null;
		Std.is(n, Array);
		new haxe.ds.Vector<Int>(10);
	}
}
