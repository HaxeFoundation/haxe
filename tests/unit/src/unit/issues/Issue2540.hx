package unit.issues;

class Issue2540 extends Test
{
    function test() {
			var r = new T();
			eq(42,r.whatever());
			var r:Foo<Dynamic> = new T();
			eq(42,r.whatever());
    }
}

private interface Foo<T>
{
	function whatever<X>():Int;
}


private class T implements Foo<Dynamic> {
	public function new()
	{
	}

	public function whatever<X>():Int
	{
		return 42;
	}
}
