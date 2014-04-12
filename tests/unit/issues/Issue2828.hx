package unit.issues;

class Issue2828 extends unit.Test
{
	public function test()
	{
		var u = new U1();
		u.foo(1,1);
		t(u.didCall);
	}
}

private interface T1<X>
{
  public function foo <A>(a:X, c:A):Void;
}

private class U1 implements T1<Dynamic>
{
  public function new () {}
	public var didCall:Bool = false;

  public function foo <A>(a:Dynamic, c:A):Void
	{
		didCall = true;
	}
}
