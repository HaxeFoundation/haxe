class Foo {
	public static function bar() {
		trace("Hello from Foo.hx");
		var foo:Bool = true;
		test();
	}

	static macro function test();
}
