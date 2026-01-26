package unit.issues;

class Issue12530 extends unit.Test {
#if lua
	// Table.iterator() and Table.keyValueIterator() for native for-loop syntax
	function testTableValueIterator() {
		var tbl:lua.Table<String, Int> = lua.Table.create();
		untyped tbl["a"] = 1;
		untyped tbl["b"] = 2;
		untyped tbl["c"] = 3;

		// Test value iterator: for (v in table)
		var sum = 0;
		for (v in tbl) {
			sum += v;
		}
		eq(sum, 6);
	}

	function testTableKeyValueIterator() {
		var tbl:lua.Table<String, Int> = lua.Table.create();
		untyped tbl["a"] = 1;
		untyped tbl["b"] = 2;
		untyped tbl["c"] = 3;

		// Test key-value iterator: for (k => v in table)
		var keys:Array<String> = [];
		var values:Array<Int> = [];
		for (k => v in tbl) {
			keys.push(k);
			values.push(v);
		}
		eq(keys.length, 3);
		eq(values.length, 3);
		// Order is undefined, so check that all values are present
		t(keys.contains("a"));
		t(keys.contains("b"));
		t(keys.contains("c"));
		t(values.contains(1));
		t(values.contains(2));
		t(values.contains(3));
	}

	function testTableEmptyIteration() {
		var empty:lua.Table<String, Int> = lua.Table.create();
		var emptyCount = 0;
		for (v in empty) {
			emptyCount++;
		}
		eq(emptyCount, 0);
	}
#end
}
