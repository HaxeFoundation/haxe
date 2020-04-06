package unit.issues;

class Issue4285 extends Test {
	function test() {
		var a = [1, 2, 3];
		t(myIs(a, Array));
	}

	static function myIs(d:Dynamic, t:Dynamic) {
		return Std.isOfType(d, t);
	}
}