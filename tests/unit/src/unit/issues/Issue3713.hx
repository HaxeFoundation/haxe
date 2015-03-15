package unit.issues;

private class A<T> {
    public function new() {}
}

private abstract B<T>(A<T>) {
    public inline function new() this = new A();
    public inline function f():C<T> return new C(this);
}

private class C<T> {
    public inline function new(d:A<T>) {}
}

class Issue3713 extends Test {
	function test() {
        var b = new B<Int>();
        var c = b.f();
	}
}