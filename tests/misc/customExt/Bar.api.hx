class Bar {
	public static function bar() {
		trace("Hello from Bar.hx");
		test();
	}

	static macro function test() return macro trace("Hello from Bar.hx macro");
}
