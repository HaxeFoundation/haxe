package unit;

class TestWeakRef extends Test {
	function testWeakRefGet() {
		var obj = {value: 42};
		var ref = new haxe.ds.WeakRef(obj);
		eq(ref.get().value, 42);
	}

	function testWeakRefIdentity() {
		var obj = {value: 1};
		var ref = new haxe.ds.WeakRef(obj);
		t(ref.get() == obj);
	}

	function testWeakMapSetGet() {
		var wm = new haxe.ds.WeakMap();
		var key1 = {id: 1};
		var key2 = {id: 2};
		wm.set(key1, "one");
		wm.set(key2, "two");
		eq(wm.get(key1), "one");
		eq(wm.get(key2), "two");
	}

	function testWeakMapExists() {
		var wm = new haxe.ds.WeakMap();
		var key = {id: 1};
		f(wm.exists(key));
		wm.set(key, "val");
		t(wm.exists(key));
	}

	function testWeakMapRemove() {
		var wm = new haxe.ds.WeakMap();
		var key = {id: 1};
		f(wm.remove(key));
		wm.set(key, "val");
		t(wm.remove(key));
		f(wm.exists(key));
	}

	function testWeakMapOverwrite() {
		var wm = new haxe.ds.WeakMap();
		var key = {id: 1};
		wm.set(key, "first");
		wm.set(key, "second");
		eq(wm.get(key), "second");
	}

	function testWeakMapMissing() {
		var wm = new haxe.ds.WeakMap();
		var key = {id: 1};
		eq(wm.get(key), null);
	}

	function testWeakMapClear() {
		var wm = new haxe.ds.WeakMap();
		var key = {id: 1};
		wm.set(key, "val");
		wm.clear();
		f(wm.exists(key));
	}

	#if !js
	function testWeakMapIteration() {
		var wm = new haxe.ds.WeakMap();
		var key1 = {id: 1};
		var key2 = {id: 2};
		wm.set(key1, "one");
		wm.set(key2, "two");

		var count = 0;
		var foundOne = false;
		var foundTwo = false;
		for (v in wm) {
			if (v == "one")
				foundOne = true;
			if (v == "two")
				foundTwo = true;
			count++;
		}
		eq(count, 2);
		t(foundOne);
		t(foundTwo);
	}

	function testWeakMapKeys() {
		var wm = new haxe.ds.WeakMap();
		var key1 = {id: 1};
		var key2 = {id: 2};
		wm.set(key1, "one");
		wm.set(key2, "two");

		var count = 0;
		for (_ in wm.keys()) {
			count++;
		}
		eq(count, 2);
	}

	function testWeakMapSize() {
		var wm = new haxe.ds.WeakMap();
		var key1 = {id: 1};
		var key2 = {id: 2};
		wm.set(key1, "one");
		wm.set(key2, "two");
		eq(wm.size(), 2);
	}
	#end

	#if js
	function testWeakMapJsEnumerationThrows() {
		var wm = new haxe.ds.WeakMap();
		exc(function() wm.keys());
		exc(function() wm.iterator());
		exc(function() wm.size());
	}
	#end
}
