package unit.teststd.haxe;

class TestDynamicAccess extends unit.Test {
	public function test() {
		var map = new haxe.DynamicAccess();
		f(map.exists("foo"));
		eq(map.get("foo"), null);
		eq((map["foo"] = 1), 1);
		map.set("bar", 2);
		eq(map.set("baz", 3), 3);
		t(map.exists("foo"));
		t(map.exists("bar"));
		t(map.exists("baz"));
		eq(map.get("foo"), 1);
		eq(map.get("bar"), 2);
		eq(map.get("baz"), 3);
		var values = [];
		for (key in map.keys()) {
			values.push(map[key]);
		}
		eq(values.length, 3);
		t(values[0] == 1 || values[0] == 2 || values[0] == 3);
		t(values[1] == 1 || values[1] == 2 || values[1] == 3);
		t(values[2] == 1 || values[2] == 2 || values[2] == 3);
		var keys = ["foo", "bar", "baz"];
		for (key in map.keys()) {
			t(keys.remove(key));
		}
		eq(keys.length, 0);
		t(map.remove("bar"));
		f(map.remove("bar"));
		t(map.exists("foo"));
		f(map.exists("bar"));
		t(map.exists("baz"));
		eq(map.get("bar"), null);
		eq(map["bar"], null);

		map = {test: 2};
		eq(map["test"], 2);

		var d:Dynamic<Int> = map;
		eq(d.test, 2);

		var map = new haxe.DynamicAccess();
		map["a"] = 1;
		map["b"] = 2;
		map["c"] = 3;

		var values = [];
		for (value in map) {
			values.push(value);
		}
		eq(values.length, 3);
		t(values[0] == 1 || values[0] == 2 || values[0] == 3);
		t(values[1] == 1 || values[1] == 2 || values[1] == 3);
		t(values[2] == 1 || values[2] == 2 || values[2] == 3);

		var keys = [];
		var values = [];
		for (key => value in map) {
			keys.push(key);
			values.push(value);
		}
		eq(keys.length, 3);
		t(keys[0] == "a" || keys[0] == "b" || keys[0] == "c");
		t(keys[1] == "a" || keys[1] == "b" || keys[1] == "c");
		t(keys[2] == "a" || keys[2] == "b" || keys[2] == "c");
		eq(values.length, 3);
		t(values[0] == 1 || values[0] == 2 || values[0] == 3);
		t(values[1] == 1 || values[1] == 2 || values[1] == 3);
		t(values[2] == 1 || values[2] == 2 || values[2] == 3);

	}
}
