package unit.issues;

private abstract class Foo {
    abstract public function foo(v:Int = 20):Int;
}

private class ConcreteFoo extends Foo {
    public function new() {}
    
    public function foo(v:Int = 20) {
        return v;
    }
}

class Issue11666 extends Test {
    function test() {
        final o = new ConcreteFoo();

        eq(20, o.foo());
        eq(12, o.foo(12));
        eq(7, (o:Foo).foo(7));
    }
}
