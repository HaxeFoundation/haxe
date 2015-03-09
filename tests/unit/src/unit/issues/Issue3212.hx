package unit.issues;

class Issue3212 extends Test
{
	public function test()
	{
		var b = new B();
		eq(b.f2(), null);
	}
}

private class A<T> {
	function f():T return null;
}

private class B extends A<String> {
	public function new()
	{
	}

	public function f2() {
		var v = f();
		return v;
	}
}
