package unit.issues;

class Issue6376 extends Test {
	public function test() {
		var s:Dynamic = "foo";
		var indexOf = Reflect.field(s, "indexOf");
		eq(Reflect.callMethod(s, indexOf, ["o", 0]), 1);
	}
}