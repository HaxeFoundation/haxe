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

private class TestImpl extends BaseTest implements ITest
{
	public var field(default,null):Int;
	public function new()
	{
		super();
		field = 5;
	}
}

#if !cpp @:nativeGen #end
private class BaseTest
{
	public function new()
	{

	}
}
