package unit.issues;

class Issue2061 extends Test
{
	function test()
	{
		eq("Hello",FinalTest.hello);
		eq("World",FinalTest.world);
		eq("Hello World",FinalTest.someValue);
		var v = new FinalTest();
		eq("Const Value", v.ctorValue);
		eq("Hello World", v.ctorValue2);
	}
}

class FinalTest
{
	@:readOnly public static var hello(default,never) = "Hello";
	@:readOnly public static var world(default,never) = "World";
	@:readOnly public static var someValue(default,never) = getSomeValue();

	@:readOnly public var ctorValue(default,never) = "Const Value";
	@:readOnly public var ctorValue2(default,never) = getSomeValue();

	public function new()
	{
	}

	private static function getSomeValue()
	{
		return hello + " " + world;
	}
}
