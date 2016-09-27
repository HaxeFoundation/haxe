package unit.issues;

class Issue3788 extends Test
{
	public function test()
	{
		var s =new Something();
		eq(s.f(), null); // if no warning is on haxe output, it passed
	}
}

private class Something<T:C>
{
	public function new()
	{
	}

	public function f() {
		var v:C = (null : T);
		return v;
	}
}

private class C {}
