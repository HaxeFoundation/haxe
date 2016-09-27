package unit.issues;

class Issue4001 extends Test
{
	public function test()
	{
		var b = new BugTest(0);
		eq(b.val,0);
		b = new BugTest(1);
		eq(b.val,1);
	}
}

class BugTest
{
	public var val:Float;
  public function new(x:Float)
	{
		this.val = x;
  }
}
