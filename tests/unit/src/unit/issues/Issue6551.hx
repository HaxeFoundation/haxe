package unit.issues;

class Issue6551 extends unit.Test {
    function test () {
        var v:Dynamic = "foo";
        v += v;
		eq(v, "foofoo");
    }
}