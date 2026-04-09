package unit.issues;

class Issue12876 extends Test {
    function test() {
        final tv = {
            fun : () -> "foo",
        };
        final v = Std.string(tv);
        t(v != null);
    }
}
