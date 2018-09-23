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
		var c2Dyn:Dynamic = c2;
		eq(2, Reflect.callMethod(c1, c2.getI, []));
		eq(2, Reflect.callMethod(c2, Reflect.field(c2, "getI"), []));
		eq(2, Reflect.callMethod(c2, c2Dyn.getI, []));

		// On targets that don't have native closures, the first argument
		// replaces the context. On other targets it is ignored.
		var cCheck = #if (js || neko || lua) c2 #else c1 #end;
		eq(2, Reflect.callMethod(cCheck, Reflect.field(c2, "getI"), []));
		eq(2, Reflect.callMethod(cCheck, c2Dyn.getI, []));
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