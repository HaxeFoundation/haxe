class Main {
	static function main() {
		trace(Foo.make());
	}
}

abstract Foo(Int) from Int {
	public static function make():Foo {
		var foo:Foo = 0;
		foo.init();
		return foo;
	}

	var a(never, set):Int;
	var b(never, set):Int;

	function init():Foo {
		a = 1;
		return this;
	}

	inline function set_a(v)
		return b = v;

	inline function set_b(v)
		return this = v;
}