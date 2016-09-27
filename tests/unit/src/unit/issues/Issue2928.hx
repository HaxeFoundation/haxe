package unit.issues;

class Issue2928 extends Test {
	function test() {
		var r:{?a:Int} = {};
		f(Reflect.hasField(r, "a"));
		eq(null, r.a);
		eq(0, Reflect.fields(r).length);
	}
}