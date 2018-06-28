package unit.issues;

class Issue7230 extends unit.Test {
	function test() {
		var data = {"$id": 123};
		eq(123, Reflect.field(data, "$id"));
	}
}
