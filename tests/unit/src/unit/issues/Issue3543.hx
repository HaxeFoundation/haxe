package unit.issues;

class Issue3543 extends Test {
    function test() {
        var a = Std.int((3 : UInt) / 2);
        eq(1, a);
    }
}
