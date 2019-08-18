package unit.issues;

using unit.issues.Issue7142;

class Issue7142 extends unit.Test {
	function test() {
		var map = ["test" => 1];
		var smap = new haxe.ds.StringMap();
		smap.set("test", 1);

		eq(1, map.doSomething());
		eq(1, smap.doSomething());
		eq(1, map.doSomethingElse());
		eq(1, smap.doSomethingElse());
	}

	static function doSomething(sm:haxe.ds.StringMap<Int>) {
		return sm.get("test");
	}

	static function doSomethingElse(sm:Map<String, Int>) {
		return sm.get("test");
	}
}