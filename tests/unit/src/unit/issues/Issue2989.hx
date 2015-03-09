package unit.issues;

class Issue2989 extends Test
{
	public function test()
	{
		Std.is(null,Array);
		new haxe.ds.Vector<Int>(10);
	}
}
