package unit.issues;

private class Child extends unit.issues.misc.Issue3133Class {
    override function test(unit:String) {
        super.test(unit);
    }
}

class Issue3133 extends Test {
	function test() {
        var t = new Child();
        t.test("foo");
	}
}