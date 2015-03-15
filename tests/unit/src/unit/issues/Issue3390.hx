package unit.issues;

private class A {
    static function a() Issue3390.privMethod();
}

private class B {
    static function b() Issue3390.privMethod();
}

@:access(unit.issues.A, unit.issues.B)
@:allow(unit.issues.A, unit.issues.B)
class Issue3390 extends Test {
    static function privMethod() {}
    function test() {
        A.a();
        B.b();
    }
}
