package unit.issues;

class Issue2304 extends unit.Test
{
	public function test()
	{
		eq(5,doTest(new TestImpl()));
	}

	static function doTest(o:ITest) return o.field;
}
private interface ITest
{
	var field(default,null):Int;
}
private class TestImpl implements ITest
{
	public var field(default,null):Int = 5;
	public function new()
	{

	}
}
