package unit.issues;

class Issue3030 extends Test
{
	function test()
	{
		var s = Something;
		var fn = s.somefunc;
		eq(fn(),42);
	}
}

@:keep private class Something
{
	public static function somefunc()
	{
		return 42;
	}
}
