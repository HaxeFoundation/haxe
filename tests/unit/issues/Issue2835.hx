package unit.issues;

class Issue2835 extends unit.Test
{
	private function test()
	{
		var t = new Test(42);
		eq(42,t.get());
	}
}

private class Test<T:(Float)>
{
	public var value:T;
	public function new(value)
	{
		this.value = value;
	}

	public function get():T
	{
		return value;
	}
}
