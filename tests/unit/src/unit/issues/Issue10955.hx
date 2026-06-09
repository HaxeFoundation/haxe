package unit.issues;

class Issue10955 extends Test {
	function test() {
		eq("false|", foo());
		eq("false|Haxe is great!", foo("Haxe is great!"));
		eq("false|Haxe is,great!", foo("Haxe is", "great!"));
		eq("true|a,b,c", foo(true, "a", "b", "c"));
	}

	static function foo(?b:Bool = false, ...args:String) {
		return (b ? "true" : "false") + "|" + args.toArray().join(",");
	}
}
