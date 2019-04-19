package unit.issues;

class Issue8053 extends unit.Test {
	function test() {
		var a:Dynamic = { };
		var b = "";
		Reflect.setField(a, b, 1);
		eq(1, Reflect.field(a, b));
	}
}