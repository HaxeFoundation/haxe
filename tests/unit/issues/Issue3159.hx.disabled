package unit.issues;

class Issue3159 extends Test
{
	public function test()
	{
		var dmet = new DynamicMethod();
		f(dmet.hasCalled);
		var test:IHasDynamicMethod = dmet;
		test.foo();
		t(dmet.hasCalled);
		test.foo = function() dmet.hasCalled = false;
		test.foo();
		f(dmet.hasCalled);
	}
}

interface IHasDynamicMethod
{
  dynamic function foo():Void;
}

private class DynamicMethod implements IHasDynamicMethod
{
	public var hasCalled = false;
	public function new()
	{
	}

	dynamic public function foo()
	{
		hasCalled = true;
	}
}
