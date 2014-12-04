package unit.issues;

private class C<T> {
    public var f:Array<T>;
    public inline function new(v) f = [v];
}

@:forward(f)
private abstract A(C<Int>) from C<Int> {}

class Issue3586 extends unit.Test {
    function test() {
        var a:A = new C(15);
        eq(a.f[0], 15);
    }
}
