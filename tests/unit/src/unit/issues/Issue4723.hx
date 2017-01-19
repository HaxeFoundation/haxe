package unit.issues;

import haxe.Json;

class Issue4723 extends unit.Test {
	function test() {
		var t = new NoNormalMethods();
		eq(10, t.test());

		var t = new HasNormalMethod();
		eq(10, t.test());
		eq(20, t.test2());
	}
}

private class NoNormalMethods {
	public function new() {}
	public dynamic function test() return 10;
}

private class HasNormalMethod {
	public function new() {}
	public dynamic function test() return 10;
	public function test2() return 20;
}