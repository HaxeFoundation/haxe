package unit.issues;

private class A {
	public function new() {}
}

@:generic
private class B<T:haxe.Constraints.Constructible<()->Void>> extends A {
}

class Issue4457 extends Test
{

	public function test()
	{
		new B<A>();
		noAssert();
	}

}

