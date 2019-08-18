package misc.issue7098;

enum abstract Foo(Int) {
	var Value = 0;
}

class Bar {
	public static function foo(f:Foo) {}
}
