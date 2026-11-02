package issues;

private class Foo {
	public var x:Float;

	public inline function new(x)
		this.x = x;

	public inline function foo(f:Foo)
		if (f.x > x)
			empty();

	public inline function empty() {
		x *= 0;
		return this;
	}
}

class Issue10741 {
	@:js('
		var a_x = 10;
		if(Math.random() > a_x) {
			a_x = 0;
		}
		issues_Issue10741.use(a_x);
	')
	static function test() {
		var a = new Foo(10);
		a.foo(new Foo(Math.random()));
		use(a.x);
	}

	@:pure(false) static function use(f:Float) {}
}
