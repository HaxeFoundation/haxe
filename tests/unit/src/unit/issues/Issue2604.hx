package unit.issues;

class Issue2604 extends unit.Test {

	public function test() {
		var v = {"a-b": 1};
		eq(Reflect.field(v, "a-b"), 1);
	}

}