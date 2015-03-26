package unit.issues;

private class A {
    static var a:Int;
    static function f(a:Int) {}
}

class Issue3714 extends Test {
    function test() {
        eq(unit.TestType.typeErrorText(A.f), "Cannot access private field f");
        eq(unit.TestType.typeErrorText(@:privateAccess A.f), null);
        eq(unit.TestType.typeErrorText(@:privateAccess A.f(A.a)), null);
        eq(unit.TestType.typeErrorText(@:privateAccess A.f(@:noPrivateAccess A.a)), "Cannot access private field a");
    }
}
