package unit.issues;

// Two structurally identical, self-referential anonymous structures. On the hl
// target these produced distinct cyclic virtuals whose comparison made the
// genhl type caches loop forever in polymorphic compare ("Out of memory").
private typedef Foo1 = {
	function make<T>(v:T):Foo1;
}

private class FooLike1 {
	public function new() {}
	public function make<T>(v:T):FooLike1 return this;
}

private typedef Foo2 = {
	function make<T>(v:T):Foo2;
}

private class FooLike2 {
	public function new() {}
	public function make<T>(v:T):FooLike2 return this;
}

class Issue9662 extends Test {
	function test() {
		var f1:Foo1 = new FooLike1();
		var f2:Foo2 = new FooLike2();
		t(f1.make(0) != null);
		t(f2.make(0) != null);
	}
}
