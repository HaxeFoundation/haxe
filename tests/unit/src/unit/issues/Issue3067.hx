package unit.issues;

private abstract TestAbstract(Int) from Int to Int
{
	public var property(get, set):Int;

	function get_property():Int
	{
		return 5;
	}

	function set_property(i:Int):Int
	{
		return i;
	}

	function manipulateProperty():Void
	{
		property *= 1;
	}
}

class Issue3067 extends Test {
	function test() {
		var a:TestAbstract = 1;
		eq(5, a.property);
	}
}