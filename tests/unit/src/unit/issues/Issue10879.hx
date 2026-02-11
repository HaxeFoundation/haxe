package unit.issues;

class Issue10879 extends Test {
	function test() {
		#if lua
		// Test that pairsMap callback takes (key, value) and returns the mapped value.
		// Before fix, the signature was A->B->C->C (3 args) but the Lua code
		// only calls the callback with 2 args (k, v).
		var t = lua.Table.create(["hello", "world"]);

		var mapped = lua.PairTools.pairsMap(t, function(k:Int, v:String):String {
			return v + "!";
		});

		eq(mapped[1], "hello!");
		eq(mapped[2], "world!");
		#else
		noAssert();
		#end
	}
}
