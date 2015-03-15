package unit.issues;

private class A {
    public var a = 5;
    public function new() {}
}

class Issue4002 extends Test {
    function test() {
        eq(fail(new A()), 5);
    }

    function fail<T:A>(v:T):Int {
        var x = function() return v.a;
        return x();
    }
}
