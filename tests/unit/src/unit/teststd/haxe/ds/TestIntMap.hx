package unit.teststd.haxe.ds;

class TestIntMap extends unit.Test {
	public function test() {
		var map1 = new haxe.ds.IntMap();
		t((map1 is haxe.ds.IntMap));
		map1.set(1, 2);
		map1.set(2, 4);
		map1.set(3, 6);
		eq(map1.get(1), 2);
		eq(map1.get(2), 4);
		t(map1.get(3) != 8);

		// iterator
		var keys1a = [for (k in map1.keys()) k];
		keys1a.sort(Reflect.compare); // Order is undefined
		aeq([1, 2, 3], keys1a);

		var values1a = [for (v in map1) v];
		values1a.sort(Reflect.compare);
		aeq([2, 4, 6], values1a);

		// key value iterator
		var keys1b = [for (k=>v in map1) k];
		keys1b.sort(Reflect.compare);
		aeq([1, 2, 3], keys1b);

		var values1b = [for (k=>v in map1) v];
		values1b.sort(Reflect.compare);
		aeq([2, 4, 6], values1b);

		var values1c = [for (k=>v in map1) k*v];
		values1c.sort(Reflect.compare);
		aeq([2, 8, 18], values1c);


		var map2 = new haxe.ds.IntMap();
		t((map2 is haxe.ds.IntMap));
		map2.set(1, "2");
		map2.set(2, "4");
		map2.set(3, "6");
		eq(map2.get(1), "2");
		eq(map2.get(2), "4");
		t(map2.get(3) != "8");

		// iterator
		var keys2a = [for (k in map2.keys()) k];
		keys2a.sort(Reflect.compare);
		aeq([1, 2, 3], keys2a);

		var values2a = [for (v in map2) v];
		values2a.sort(Reflect.compare);
		aeq(["2", "4", "6"], values2a);
		// key value iterator
		var keys2b = [for (k=>v in map2) k];
		keys2b.sort(Reflect.compare);
		aeq([1, 2, 3], keys2b);

		var values2b = [for (k=>v in map2) v];
		values2b.sort(Reflect.compare);
		aeq(["2", "4", "6"], values2b);

		// clear
		map2.clear();
		eq(map2.get(1), null);
		f(map2.exists(1));
		f(map2.exists(2));
		f(map2.exists(3));

		var keys3 = [for (k in map2.keys()) k];
		eq(keys3.length, 0);


		// Test unification

		var map:haxe.ds.IntMap<String> = [1=>"2",2=>"4"];
		var iterable:KeyValueIterable<Int, String> = map;
		var iterator:KeyValueIterator<Int,String> = iterable.keyValueIterator();
		var values = [for(kv in iterator) kv.value];
		t(values[0] == "2" || values[0] == "4");
		t(values[1] == "2" || values[1] == "4");
		var iterator:KeyValueIterator<Int,String> = iterable.keyValueIterator();
		var keys = [for(kv in iterator) kv.key];
		t(keys[0] == 1 || keys[0] == 2);
		t(keys[1] == 1 || keys[1] == 2);

	}
}
