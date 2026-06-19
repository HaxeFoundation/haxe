package unit.issues;

class Issue10808 extends unit.Test {
	function test() {
		eq("a,b in test", log("a", "b"));
		eq(" in test", log());
	}

	// haxe.Rest and a trailing haxe.PosInfos as last arguments
	static function log(...rest:String, ?pos:haxe.PosInfos):String {
		return rest.toArray().join(",") + " in " + pos.methodName;
	}
}
