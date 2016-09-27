package unit.issues;

class Issue3466 extends Test {
	function test() {
		var d:Dynamic<Int> = {};
		eq(null, Reflect.field(d,'someField'));
	}
}
