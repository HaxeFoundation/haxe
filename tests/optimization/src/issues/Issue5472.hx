package issues;

private class Foo {
	var v:String;

	public inline function new(v:String)
		this.v = v;

	public inline function setFrom(another:Foo):Foo {
		Issue5472.use(v, another.v);
		v = another.v;
		return new Foo(v);
	}
}

class Issue5472 {
	@:js('
		issues_Issue5472.use(v2,v);
	')
	static function test0(v:String, v2:String) {
		var f = new Foo(v);
		var f2 = new Foo(v2);
		f2.setFrom(f);
	}

	@:js('
		issues_Issue5472.use(v2,v);
	')
	static function test1(v:String, v2:String) {
		var f = new Foo(v);
		var f2 = new Foo(v2);
		var f3 = f2.setFrom(f);
	}

	@:js('
		issues_Issue5472.use(v2,v);
		issues_Issue5472.use("f4",v);
	')
	static function test2(v:String, v2:String) {
		var f = new Foo(v);
		var f2 = new Foo(v2);
		var f3 = f2.setFrom(f);
		var f4 = new Foo("f4");
		var t = f4.setFrom(f3);
	}

	@:pure(false) public static function use(a:String, b:String) {}
}
