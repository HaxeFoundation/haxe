package unit.issues;

private class C {
	var i:Int;

	public function new(i:Int) {
		this.i = i;
	}

	public function getI() {
		return i;
	}
}

class Issue7106 extends unit.Test {
	function testClosure() {
		var c1 = new C(1);
		var c2 = new C(2);
		eq(1, Reflect.callMethod(c1, c2.getI, []));
		eq(1, Reflect.callMethod(c1, Reflect.field(c2, "getI"), []));
	}

	function testStatic() {
        eq(49, Reflect.callMethod(null,foo,[]));
        eq(49, Reflect.callMethod(Issue7106,foo,[]));
        eq(49, Reflect.callMethod(Type,foo,[]));
	}

	static function foo() {
		return 49;
	}
}