package unit.issues;

import haxe.ds.StringMap;
class Issue5945 extends Test {
	function test() {
		var test_map : StringMap<String> = new StringMap<String>();
		var test_key : String = "key";
		test_map.set(test_key, "Hello world");
		eq(test_map.exists(test_key) == false, false);
	}
}
