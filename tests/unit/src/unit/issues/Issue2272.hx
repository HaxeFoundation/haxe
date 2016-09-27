package unit.issues;

class Issue2272 extends unit.Test
{
	public function test()
	{
		var ex = new Example();
		eq(5, Reflect.field(ex,'f'));
	}
}

private class Example {
	@:isVar @:keep public var f(get, set):Int;
	function get_f() return f;
	function set_f(param) return f = param;
	public function new() {
		Reflect.setField(this, 'f', 5);
	}
}
