package unit.issues;

class Issue8337 extends unit.Test {
	function test() {
		var n:NullProperty = 0;
		if (n.foo == null) {}
		noAssert();
	}
}

abstract NullProperty(Int) from Int {
	public var foo(get, never):Null<Int>;

	function get_foo():Null<Int> {
		return 0;
	}
}