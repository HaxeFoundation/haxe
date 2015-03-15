package unit.issues;
import haxe.ds.StringMap;

#if (java || cs)

class Issue3173 extends Test
{
	public function test()
	{
		var o2 = new O2();
		eq(o2.foo(null), 1);
		var o:O<StringMap<Int>> = o2;
		eq(o.foo(null), 1);
	}
}

private class O2 extends O<StringMap<Int>>
{
	public function new()
	{
	}

#if (java || cs)
	@:overload
#end
	override public function foo(t:StringMap<Int>):Int
	{
		return 1;
	}
}

private class O<T>
{
#if (java || cs)
	@:overload
#end
	public function foo(t:T):Int
	{
		return 0;
	}
}

#else

class Issue3173 extends Test { }

#end