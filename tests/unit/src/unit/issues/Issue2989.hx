package unit.issues;

class Issue2989 extends Test
{
	public function test()
	{
		var n = null;
		(n is Array);
		new haxe.ds.Vector<Int>(10);
	}
}
