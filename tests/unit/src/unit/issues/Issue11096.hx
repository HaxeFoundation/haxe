package unit.issues;

private class Macro {
	public macro static function foo(_i:Int) {
		static var i:Int;
		var r = i;
		i = _i;
		return macro $v{"" + r}
	}
}

class Issue11096 extends Test {
	function test() {
		eq("null", Macro.foo(5));
		// Haxe 6: the statics of macros, local ones included, are reset for
		// every expansion (docs/incremental-compilation.md, phase 7)
		eq(#if macro_isolation "null" #else "5" #end, Macro.foo(2));
	}
}
