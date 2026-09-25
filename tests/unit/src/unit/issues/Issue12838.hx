package unit.issues;

class Issue12838 extends Test {
	function test() {
		eq(Reflect.callMethod(null, Reflect.field(String, "fromCharCode"), [65]), "A");
	}
}
