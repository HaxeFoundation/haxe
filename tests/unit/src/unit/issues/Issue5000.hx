package unit.issues;

class Issue5000 extends Test {
    function test() {
        var a = {"a\n b": 1};
        eq(1, Reflect.field(a, "a\n b"));
    }
}
