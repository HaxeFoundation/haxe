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

	function testOverload() {
		eq("opt:null|", bar());
		eq("opt:null|a", bar("a"));
		eq("opt:null|a,b", bar("a", "b"));
		eq("opt:true|a,b", bar(true, "a", "b"));
		eq("int:3|a,b", bar(3, "a", "b"));
	}

	overload extern inline static function bar(?b:Bool, ...args:String) {
		return "opt:" + b + "|" + args.toArray().join(",");
	}

	overload extern inline static function bar(n:Int, ...args:String) {
		return "int:" + n + "|" + args.toArray().join(",");
	}
}
