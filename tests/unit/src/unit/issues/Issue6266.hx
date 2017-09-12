package unit.issues;

class Issue6266 extends unit.Test {

	function test() {
		var foo = "foo";
		eq(Reflect.field(foo, "length"), 3);
	}
}
