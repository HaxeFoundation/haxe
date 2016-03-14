package unit.issues;

private enum Kind {
    KA;
    KB;
}

private class Base {
    public function new() {}
}

private class A extends Base {}
private class B extends Base {}

class Issue4940 extends Test {
	function test() {
        var kind = KA;
        var base = switch (kind) {
            case KA: new A();
            case KB: new B();
        }
		unit.TestType.typedAs(new Base(), base);
	}
}