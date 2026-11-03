package unit.issues;

class Issue10921 extends Test {
	#if lua
	function test() {
		// Test that calling a function field of a multi-return local variable
		// generates the correct variable name (e.g., _hx_1_p_next instead of p.next)
		var t:lua.Table<Int, String> = untyped __lua__("{[1] = 'a', [2] = 'b', [3] = 'c'}");
		var result = findNext(t, function(s) return s == "b");
		eq(result, "b");

		// Also test with pairs
		var result2 = findFirst(t, function(s) return s == "c");
		eq(result2, "c");
	}

	static function findNext<T>(table:lua.Table<Int, T>, fn:T->Bool):Null<T> {
		final p = lua.Lua.ipairs(table);
		function loop(nextP:lua.Lua.NextResult<Int, T>) {
			return if (fn(nextP.value))
				nextP.value
			else
				loop(p.next(p.table, nextP.index));
		}
		return loop(p.next(p.table, p.index));
	}

	static function findFirst<K, V>(table:lua.Table<K, V>, fn:V->Bool):Null<V> {
		final p = lua.Lua.pairs(table);
		function loop(nextP:lua.Lua.NextResult<K, V>) {
			if (nextP.index == null)
				return null;
			return if (fn(nextP.value))
				nextP.value
			else
				loop(p.next(p.table, nextP.index));
		}
		return loop(p.next(p.table, p.index));
	}
	#end
}
