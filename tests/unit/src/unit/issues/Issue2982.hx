package unit.issues;

class Issue2982 extends Test {

	function test() {
		eq(12, run());
	}

    static macro function run() {
        var map:Map<String,Int> = new Map();
		map["foo"] = 12;
        var sMap:haxe.ds.StringMap<Int> = map; // Test.hx:8: characters 2-40 : Map<String, Int> should be haxe.ds.StringMap<Int>
        return macro $v{sMap.get("foo")};
    }
}