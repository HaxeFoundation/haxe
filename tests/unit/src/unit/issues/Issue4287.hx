package unit.issues;

import haxe.ds.Option;

class Issue4287 extends unit.Test {
	function test() {
		eq(None, getNone());
	}

	static function getNone(t = None) {
		return t;
	}
}