package unit.issues;

class Issue6294 extends unit.Test {
    public function test() {
        (cast (new Foo():{})).handle();
        t(true);
    }
}

@:keep
private class Foo {
    public function new() {}
    public function handle() {}
}