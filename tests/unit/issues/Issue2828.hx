package unit.issues;

class Issue2828 extends unit.Test
{
	public function test()
	{
		var u:T1<Dynamic> = new U1();
		u.foo(1,1);
		t(u.call());
	}
}

private interface T1<X>
{
  public function foo <A>(a:X, c:A):Void;
	public function call():Bool;
}

private class U1 implements T1<Dynamic>
{
  public function new () {}
	public var didCall:Bool = false;

	public function call()
	{
		return didCall;
	}

  public function foo <A>(a:Dynamic, c:A):Void
	{
		didCall = true;
	}
}
