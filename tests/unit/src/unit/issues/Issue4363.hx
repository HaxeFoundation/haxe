package unit.issues;

class Issue4363 extends Test
{
	public function test()
	{
		var i:I<C> = new C();
		eq(null, i.f());
		eq(i, i.f2());
	}
}

private interface I<T> {
	function f(?arg:Float):T;
	function f2(?arg:Float):T;
}

private class C implements I<C> {
	public function new()
	{
	}

	public function f(?arg:Float):C
	{
		return null;
	}

	public function f2(?arg:Float):C
	{
		return this;
	}
}
