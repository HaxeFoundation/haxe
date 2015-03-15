package unit.issues;

class Issue3217 extends Test
{
	public function test()
	{
		var t:{ test:Int } = new NativeClass();
		t.test = 10;
		eq(t.test,10);
	}
}

@:nativeGen
private class NativeClass
{
  public var test:Int;
  public function new() {}
}
