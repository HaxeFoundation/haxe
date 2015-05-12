package unit.issues;

class Issue2570 extends unit.Test
{
	public function test()
	{
		eq(42,new B().value);
		eq(21,new C().value);
	}
}

#if !cpp @:nativeGen #end private class A
{
	public function new()
	{
	}
}

private class B extends A
{
	public var value:Int;
	public function new(val=42)
	{
		super();
		this.value = val;
	}
}

private class C extends B
{
	public function new(val=21)
	{
		super(val);
	}
}
