package unit.issues;
class Issue5973 extends Test{
	public function test(){
		var foo    = new Issue5973Foo();
		var bar    = new Issue5973Bar();
		var foobar = new Issue5973FooBar();
		t(Std.is(foo    , Issue5973IFoo));
		f(Std.is(foo    , Issue5973IBar));
		f(Std.is(bar    , Issue5973IFoo));
		t(Std.is(bar    , Issue5973IBar));
		t(Std.is(foobar , Issue5973IFoo));
		t(Std.is(foobar , Issue5973IBar));
	}
}

interface Issue5973IFoo {
	function foo() : Void;
}

interface Issue5973IBar {
	function bar() : Void;
}

class Issue5973Foo implements Issue5973IFoo {
	public function new() {}
	public function foo() {}
}

class Issue5973Bar implements Issue5973IBar {
	public function new() {}
	public function bar() {}
}

class Issue5973FooBar implements Issue5973IFoo implements Issue5973IBar {
	public function new() {}
	public function foo() {}
	public function bar() {}
}
