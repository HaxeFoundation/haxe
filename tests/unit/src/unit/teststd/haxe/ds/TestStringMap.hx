package unit.teststd.haxe.ds;

class TestStringMap extends unit.Test {
	public function test() {
		var map1 = new haxe.ds.StringMap();
		t((map1 is haxe.ds.StringMap));
		map1.set("1a", 2);
		map1.set("2a", 4);
		map1.set("3a", 6);
		eq(map1.get("1a"), 2);
		eq(map1.get("2a"), 4);
		t(map1.get("3a") != 8);

		// iterator
		var keys1a = [for (k in map1.keys()) k];
		keys1a.sort(Reflect.compare); // Order is undefined
		aeq(["1a", "2a", "3a"], keys1a);

		var values1a = [for (v in map1) v];
		values1a.sort(Reflect.compare);
		aeq([2, 4, 6], values1a);

		// key value iterator
		var keys1b = [for (k=>v in map1) k];
		keys1b.sort(Reflect.compare);
		aeq(["1a", "2a", "3a"], keys1b);

		var values1b = [for (k=>v in map1) v];
		values1b.sort(Reflect.compare);
		aeq([2, 4, 6], values1b);


		var map2 = new haxe.ds.StringMap();
		t((map2 is haxe.ds.StringMap));
		map2.set("1a", "2");
		map2.set("2a", "4");
		map2.set("3a", "6");
		eq(map2.get("1a"), "2");
		eq(map2.get("2a"), "4");
		t(map2.get("3a") != "8");

		// iterator
		var keys2a = [for (k in map2.keys()) k];
		keys2a.sort(Reflect.compare);
		aeq(["1a", "2a", "3a"], keys2a);

		var values2a = [for (v in map2) v];
		values2a.sort(Reflect.compare);
		aeq(["2", "4", "6"], values2a);
		// key value iterator
		var keys2b = [for (k=>v in map2) k];
		keys2b.sort(Reflect.compare);
		aeq(["1a", "2a", "3a"], keys2b);

		var values2b = [for (k=>v in map2) v];
		values2b.sort(Reflect.compare);
		aeq(["2", "4", "6"], values2b);

		// clear
		map2.clear();
		eq(map2.get("1a"), null);
		f(map2.exists("1a"));
		f(map2.exists("2a"));
		f(map2.exists("3a"));

		var keys3 = [for (k in map2.keys()) k];
		eq(keys3.length, 0);

		// Test unification

		var map:haxe.ds.StringMap<String> = ["1a"=>"2","1b"=>"4"];
		var iterable:KeyValueIterable<String, String> = map;
		var iterator:KeyValueIterator<String,String> = iterable.keyValueIterator();
		var values = [for(kv in iterator) kv.value];
		t(values[0] == "2" || values[0] == "4");
		t(values[1] == "2" || values[1] == "4");
		var iterator:KeyValueIterator<String,String> = iterable.keyValueIterator();
		var keys = [for(kv in iterator) kv.key];
		t(keys[0] == "1a" || keys[0] == "1b");
		t(keys[1] == "1a" || keys[1] == "1b");
	}
}
