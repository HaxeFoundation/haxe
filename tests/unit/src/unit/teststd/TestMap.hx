package unit.teststd;

class TestMap extends unit.Test {
	public function test() {
		// String
		var map = new Map();
		f(map.exists("foo"));
		eq(map.get("foo"), null);
		map.set("foo", 1);
		map.set("bar", 2);
		map.set("baz", 3);
		var dynmap:Dynamic = map;
		var map2:haxe.Constraints.IMap<Dynamic,Dynamic> = dynmap;
		var map3:haxe.Constraints.IMap<String, Dynamic> = dynmap;
		var map4:haxe.Constraints.IMap<String, Int> = dynmap;
		t((map is haxe.ds.StringMap));
		t(map.exists("foo"));
		t(map.exists("bar"));
		t(map.exists("baz"));
		eq(map.get("foo"), 1);
		eq(map.get("bar"), 2);
		eq(map.get("baz"), 3);
		t(map2.exists("foo"));
		eq(map2.get("foo"), 1);
		t(map3.exists("foo"));
		eq(map3.get("foo"), 1);
		t(map4.exists("foo"));
		eq(map4.get("foo"), 1);

		var copied = map.copy();
		t(copied != map);
		eq(copied.exists("foo"), map.exists("foo"));
		eq(copied.exists("bar"), map.exists("bar"));
		eq(copied.exists("baz"), map.exists("baz"));
		eq(copied.get("foo"), map.get("foo"));
		eq(copied.get("bar"), map.get("bar"));
		eq(copied.get("baz"), map.get("baz"));

		copied.set("foo", 4);
		eq(copied.get("foo"), 4);
		eq(map.get("foo"), 1);

		var values = [];
		for (val in map) {
			values.push(val);
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

		var map3 = [1=>"2",2=>"4",3=>"6"];
		var keys = [for (k=>v in map3) k];
		keys.sort(Reflect.compare);
		aeq([1, 2, 3], keys);
		var values = [for (k=>v in map3) v];
		values.sort(Reflect.compare);
		aeq(["2", "4", "6"], values);


		// Int
		var map = new Map();
		f(map.exists(1));
		eq(map.get(1), null);
		map.set(1, 1);
		map.set(2, 2);
		map.set(3, 3);
		t((map is haxe.ds.IntMap));
		t(map.exists(1));
		t(map.exists(2));
		t(map.exists(3));
		eq(map.get(1), 1);
		eq(map.get(2), 2);
		eq(map.get(3), 3);

		var copied = map.copy();
		t(copied != map);
		eq(copied.exists(1), map.exists(1));
		eq(copied.exists(2), map.exists(2));
		eq(copied.exists(3), map.exists(3));
		eq(copied.get(1), map.get(1));
		eq(copied.get(2), map.get(2));
		eq(copied.get(3), map.get(3));

		copied.set(1, 4);
		eq(copied.get(1), 4);
		eq(map.get(1), 1);

		var values = [];
		for (val in map) {
			values.push(val);
		}
		eq(values.length, 3);
		t(values[0] == 1 || values[0] == 2 || values[0] == 3);
		t(values[1] == 1 || values[1] == 2 || values[1] == 3);
		t(values[2] == 1 || values[2] == 2 || values[2] == 3);
		var keys = [1, 2, 3];
		for (key in map.keys()) {
			t(keys.remove(key));
		}
		eq(keys.length, 0);
		t(map.remove(2));
		f(map.remove(2));
		t(map.exists(1));
		f(map.exists(2));
		t(map.exists(3));
		eq(map.get(2), null);

		var map3 = [1=>2,2=>4,3=>6];
		var keys = [for (k=>v in map3) k];
		keys.sort(Reflect.compare);
		aeq([1, 2, 3], keys);
		var values = [for (k=>v in map3) v];
		values.sort(Reflect.compare);
		aeq([2, 4, 6], values);

		// Hashable
		var map = new Map();
		var a = new unit.MyAbstract.ClassWithHashCode(1);
		var b = new unit.MyAbstract.ClassWithHashCode(2);
		var c = new unit.MyAbstract.ClassWithHashCode(3);
		f(map.exists(a));
		eq(map.get(a), null);
		map.set(a, 1);
		map.set(b, 2);
		map.set(c, 3);
		t(map.exists(a));
		t(map.exists(b));
		t(map.exists(c));
		eq(map.get(a), 1);
		eq(map.get(b), 2);
		eq(map.get(c), 3);

		var keys = [for (k=>v in map) k];
		t(keys[0] == a || keys[0] == b || keys[0] == c);
		t(keys[1] == a || keys[1] == b || keys[1] == c);
		t(keys[2] == a || keys[2] == b || keys[2] == c);
		var values = [for (k=>v in map) v];
		t(values[0] == 1 || values[0] == 2 || values[0] == 3);
		t(values[1] == 1 || values[1] == 2 || values[1] == 3);
		t(values[2] == 1 || values[2] == 2 || values[2] == 3);

		var copied = map.copy();
		t(copied != map);
		eq(copied.exists(a), map.exists(a));
		eq(copied.exists(b), map.exists(b));
		eq(copied.exists(c), map.exists(c));
		eq(copied.get(a), map.get(a));
		eq(copied.get(b), map.get(b));
		eq(copied.get(c), map.get(c));

		copied.set(a, 4);
		eq(copied.get(a), 4);
		eq(map.get(a), 1);

		var values = [];
		for (val in map) {
			values.push(val);
		}
		eq(values.length, 3);
		t(values[0] == 1 || values[0] == 2 || values[0] == 3);
		t(values[1] == 1 || values[1] == 2 || values[1] == 3);
		t(values[2] == 1 || values[2] == 2 || values[2] == 3);
		var keys = [a, b, c];
		for (key in map.keys()) {
			t(keys.remove(key));
		}
		eq(keys.length, 0);
		t(map.remove(b));
		f(map.remove(b));
		t(map.exists(a));
		f(map.exists(b));
		t(map.exists(c));
		eq(map.get(b), null);

		// Object
		var map = new Map();
		var a = new unit.MyAbstract.ClassWithoutHashCode(1);
		var b = new unit.MyAbstract.ClassWithoutHashCode(2);
		var c = new unit.MyAbstract.ClassWithoutHashCode(3);
		f(map.exists(a));
		eq(map.get(a), null);
		map.set(a, 1);
		map.set(b, 2);
		map.set(c, 3);
		t(map.exists(a));
		t(map.exists(b));
		t(map.exists(c));
		eq(map.get(a), 1);
		eq(map.get(b), 2);
		eq(map.get(c), 3);

		var keys = [for (k=>v in map) k];
		t(keys[0] == a || keys[0] == b || keys[0] == c);
		t(keys[1] == a || keys[1] == b || keys[1] == c);
		t(keys[2] == a || keys[2] == b || keys[2] == c);
		var values = [for (k=>v in map) v];
		t(values[0] == 1 || values[0] == 2 || values[0] == 3);
		t(values[1] == 1 || values[1] == 2 || values[1] == 3);
		t(values[2] == 1 || values[2] == 2 || values[2] == 3);

		var copied = map.copy();
		t(copied != map);
		eq(copied.exists(a), map.exists(a));
		eq(copied.exists(b), map.exists(b));
		eq(copied.exists(c), map.exists(c));
		eq(copied.get(a), map.get(a));
		eq(copied.get(b), map.get(b));
		eq(copied.get(c), map.get(c));

		copied.set(a, 4);
		eq(copied.get(a), 4);
		eq(map.get(a), 1);

		var values = [];
		for (val in map) {
			values.push(val);
		}
		eq(values.length, 3);
		t(values[0] == 1 || values[0] == 2 || values[0] == 3);
		t(values[1] == 1 || values[1] == 2 || values[1] == 3);
		t(values[2] == 1 || values[2] == 2 || values[2] == 3);
		var keys = [a, b, c];
		for (key in map.keys()) {
			t(keys.remove(key));
		}
		eq(keys.length, 0);
		t(map.remove(b));
		f(map.remove(b));
		t(map.exists(a));
		f(map.exists(b));
		t(map.exists(c));
		eq(map.get(b), null);

		// [] access
		var map = new Map();
		eq(map["foo"], null);
		map["foo"] = 12;
		eq(map.get("foo"), 12);
		eq(map["foo"], 12);
		map["foo"] += 2;
		eq(map.get("foo"), 14);
		eq(map["foo"], 14);
		map["foo"] *= map["foo"] + 2;
		eq(map["foo"], 224);
		map["f" + "o" + "o"] -= 223;
		eq(map[(function(s) return s + "o")("fo")], 1);
		map["bar"] = map["foo"] = 9;
		eq(map["bar"], 9);
		eq(map["foo"], 9);

		eq(['' => ''].keys().next(), '');
		eq(['' => ''].iterator().next(), '');
		eq([2 => 3].keys().next(), 2);
		eq([2 => 3].iterator().next(), 3);
		//[a => b].keys().next() == a;
		//[a => b].iterator().next() == b;

		var map:Map<String, Int>;
		HelperMacros.typedAs((null : Map<String, Int>), map = []);
		t(HelperMacros.typeError(map[1] = 1));

		eq(['' => ''].keyValueIterator().next().key, '');
		eq(['' => ''].keyValueIterator().next().value, '');
		eq([2 => 3].keyValueIterator().next().key, 2);
		eq([2 => 3].keyValueIterator().next().value, 3);

		// Test unification

		var map = [1=>"2",2=>"4"];
		var iterable:KeyValueIterable<Int, String> = map;
		var values = [for(kv in iterable.keyValueIterator()) kv.value];
		t(values[0] == "2" || values[0] == "4");
		t(values[1] == "2" || values[1] == "4");

		var iterator:KeyValueIterator<Int,String> = iterable.keyValueIterator();
		var keys = [for(kv in iterator) kv.key];
		t(keys[0] == 1 || keys[0] == 2);
		t(keys[1] == 1 || keys[1] == 2);


		// Test through Dynamic

		var map = [1=>"2",2=>"4"];
		var dyn:Dynamic = map;
		var it = dyn.iterator();
		var it:Iterator<String> = cast it;
		var values = [for(v in it) v];
		t(values[0] == "2" || values[0] == "4");
		t(values[1] == "2" || values[1] == "4");

		var it = dyn.keyValueIterator();
		var it:KeyValueIterator<Int,String> = cast it;
		var values = [for(kv in it) kv.value];
		t(values[0] == "2" || values[0] == "4");
		t(values[1] == "2" || values[1] == "4");
		var it = dyn.keyValueIterator();
		var it:KeyValueIterator<Int,String> = cast it;
		var keys = [for(kv in it) kv.key];
		t(keys[0] == 1 || keys[0] == 2);
		t(keys[1] == 1 || keys[1] == 2);


		var map = ["1a"=>"2","1b"=> "4"];
		var dyn:Dynamic = map;
		var it = dyn.iterator();
		var it:Iterator<String> = cast it;
		var values = [for(v in it) v];
		t(values[0] == "2" || values[0] == "4");
		t(values[1] == "2" || values[1] == "4");

		var it = dyn.keyValueIterator();
		var it:KeyValueIterator<String,String> = cast it;
		var values = [for(kv in it) kv.value];
		t(values[0] == "2" || values[0] == "4");
		t(values[1] == "2" || values[1] == "4");

		var it = dyn.keyValueIterator();
		var it:KeyValueIterator<String,String> = cast it;
		var keys = [for(kv in it) kv.key];
		t(keys[0] == "1a" || keys[0] == "1b");
		t(keys[1] == "1a" || keys[1] == "1b");

		// Test size

		eq(new Map<Int, String>().size(), 0);
		eq([1 => "a"].size(), 1);
		eq([1 => "a", 3 => "c"].size(), 2);

		eq(new Map<String, Int>().size(), 0);
		eq(["a" => 1].size(), 1);
		eq(["a" => 1, "b" => 3].size(), 2);

		eq(new Map<{a: Int}, String>().size(), 0);
		eq([{a: 1} => "a"].size(), 1);
		eq([{a: 1} => "a", {a: 3} => "c"].size(), 2);

		eq(new Map<unit.MyAbstract.ClassWithHashCode, Int>().size(), 0);
		eq([new unit.MyAbstract.ClassWithHashCode(1) => 1].size(), 1);
		eq([new unit.MyAbstract.ClassWithHashCode(1) => 1, new unit.MyAbstract.ClassWithHashCode(3) => 3].size(), 2);
	}
}
