package unit.issues;

class Issue5253 extends Test {
	static var a:Null<Int> = 1;
	static var b:Null<Int> = 2;

	function test()
	{
		var doubleValue:Float = a / b;
		eq(doubleValue, 0.5);
	}
}
