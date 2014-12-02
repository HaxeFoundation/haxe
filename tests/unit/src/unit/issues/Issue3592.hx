package unit.issues;

private class A {
    public function new() {}
    @:generic
    public function foo<T>(x : T) {
        return 'A ${x}';
    }

    @:generic
	@:extern // easy way of testing if things inline
    public inline function inlineFoo<T>(x : T) {
        return 'inline A ${x}';
    }
}

private class B extends A {
    public function new() {
        super();
    }

    @:generic
    override public function foo<T>(x : T) {
        return 'B ${x}';
    }
}

class Issue3592 extends Test {
	function test() {
        var a = new A();
        var b = new B();
        eq("A foo", a.foo("foo"));
        eq("B foo", b.foo("foo"));
		eq("inline A foo", a.inlineFoo("foo"));
	}
}