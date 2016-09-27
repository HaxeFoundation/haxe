package unit.issues;

private interface IObject {}

private class OObject implements IObject {
   public function new() {}
}

class Issue4050 extends Test {
	function test() {
		// Test 1: simple trace of an empty map
		var tmp = new Map<Int, String>();
		f(tmp.toString() == "null");
		f(tmp == null);

		// Test 2: ObjectMap with Interface as keytype and keys()-loop with value access
		var myObject = new OObject();
		var tmp2 = new Map<IObject, Array<String>>();

		tmp2.set(myObject, ["foo", "bar"]);
		for (k in tmp2.keys()) {
			var value = tmp2.get(k);
			f(value.toString() == "null");
			f(value == null);
		}
	}
}