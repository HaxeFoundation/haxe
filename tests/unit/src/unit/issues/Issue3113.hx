package unit.issues;

class Issue3113 extends Test
{
	public function test()
	{
		var t:TD = cast A;
		eq( t.func(1, 2), 1 ); // works

		t = cast B;
		eq( t.func(1, 2), 1 ); // runtime: Unhandled Exception:
		                       // Haxe Exception: Method 'func' not found on type B
		                       // [ERROR] FATAL UNHANDLED EXCEPTION: Haxe Exception: Method 'func' not found on type B
		                       // -> because of B having type parameter
	}
}

private typedef TD = { function func<T>(x:T, y:T):T; }
private class Base<T> {}

@:keep private class A
{
    public static function func<G>(x:G, y:G):G { return x; }
}

@:keep private class B<T> extends Base<T>
{
    public static function func<G>(x:G, y:G):G { return x; }
}
