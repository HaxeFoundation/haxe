package unit.issues;

private class A {
	public function new() {}
}

@:generic
private class B<T:{function new():Void;}> extends A {
}

class Issue4457 extends Test
{

	public function test()
	{
		new B<A>();
	}

}

