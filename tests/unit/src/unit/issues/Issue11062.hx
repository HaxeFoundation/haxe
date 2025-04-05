package unit.issues;

private class Foo {
	static public var flag = false;

	public var foo:Int = 5;

	public var bar(get, never):Int;

	function get_bar() {
		flag = true;
		return 4;
	}

	public function new() {}
}

class Issue11062 extends Test {
	function test() {
		var foo = new Foo();

		switch foo {
			case {foo: 123}:
				trace('yes!');
			case {foo: 321}:
				trace('yes!');
			default:
		}

		f(Foo.flag);
	}
}
