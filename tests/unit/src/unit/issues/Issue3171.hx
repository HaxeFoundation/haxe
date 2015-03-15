package unit.issues;

class Issue3171 extends Test
{
#if (java || cs)
	public function test()
	{
		var oint = new O2();
		oint.foo(10);
		eq(10,oint.lastVal);
		f(oint.called);

		var oint2:O<Int> = new O();
		oint2.foo(10);
		eq(0,oint2.lastVal);
		t(oint2.called);

		oint2 = new O2();
		oint2.foo(20);
		eq(20, oint2.lastVal);
		f(oint2.called);
	}
#end
}
#if (java || cs)
class O<T>
{
	public var lastVal = 0;
	public var called = false;
	public function new()
	{
	}

	@:overload public function foo(t:T):Void
	{
		called = true;
	}
}

class O2 extends O<Int>
{
	@:overload override public function foo(t:Int):Void
	{
		lastVal = t;
	}
}
#end
