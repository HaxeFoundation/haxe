package unit.issues;

private typedef Foo1 = {
	function make<T>(v:T):Foo1;
}

private typedef Foo2 = {
	function make<T>(v:T):Foo2;
}

private class FooLike {
	public function new() {}

	public function make<T>(v:T):FooLike
		return this;
}

private typedef ListA = {
	var next:ListA;
	var v:Int;
}

private typedef ListB = {
	var next:ListB;
	var v:Int;
}

private typedef ListC = {
	var next:ListC;
	var v:Float;
}

class Issue9662 extends Test {
	function test() {
		var foo1:Foo1 = new FooLike();
		var foo2:Foo2 = new FooLike();
		t(foo1.make(1) != null);
		t(foo2.make("a") != null);
	}

	function testSameStructure() {
		var a:ListA = {next: null, v: 1};
		var b:ListB = {next: a, v: 2};
		a.next = b;
		var c:ListA = b;
		eq(1, c.next.v);
		t(a.next == b);
		var d:Dynamic = b;
		eq(2, d.v);
		var arr:Array<Dynamic> = [a, b];
		eq(2, arr.length);
	}

	function testDifferentStructure() {
		var a:ListA = id({next: null, v: 1});
		var c:ListC = id({next: null, v: 1.5});
		eq(null, id(a).next);
		eq(null, id(c).next);
		eq(1, id(a).v);
		feq(1.5, id(c).v);
	}

	static function id<T>(v:T):T
		return v;
}
