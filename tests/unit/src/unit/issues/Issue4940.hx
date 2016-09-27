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

@:enum
private abstract K(Int) {
	var grav = 19;
	var other = 2;
}

class Issue4940 extends Test {
	function test() {
        var kind = KA;
        var base = switch (kind) {
            case KA: new A();
            case KB: new B();
        }
		unit.HelperMacros.typedAs(new Base(), base);
	}

	function testResolutionOrder() {
		var grav = "string";
		var x = switch (other) {
			case grav: 1;
			case other: 2;
		}
		eq(2, x);
	}
}