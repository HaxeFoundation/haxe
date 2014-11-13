package unit.issues;

private abstract Foo(Int) {
    public function new(x:Int) this = x;
    @:op(A+B) public static function add(a:Foo, b:Foo):Foo;
	public function get() {
		return this;
	}
}

class Issue2409 extends Test {
	function test() {
		var f:Foo = new Foo(1);
		eq(2, (f+f).get());
	}
}