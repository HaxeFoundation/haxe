package unit.issues;

class Issue2835 extends unit.Test
{
	private function test()
	{
		var t = new Test(42);
		eq(42,t.get());
		t.fromFloat(12);
		eq(12,t.get());
		eq(12.0,t.toFloat());
	}
}

private class Test<T:(Float)>
{
	public var value:T;
	public function new(value)
	{
		this.value = value;
	}

	public function toFloat():Float
	{
		return value;
	}

	public function fromFloat(v:Float)
	{
		this.value = cast v;
	}

	public function get():T
	{
		return value;
	}
}
