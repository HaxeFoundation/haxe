package unit.issues;

class Issue9883 extends unit.Test {
	function test() {
		eq("ok", silly());
	}

	static function callMe(d:Dynamic, f:Dynamic) {}

	static function silly() {
		callMe(return "ok", () -> {});
		// This is unreachable, but our control flow detection chokes on this too...
		return "not ok";
	}

	static function silly2() {
		new SillyClass(return "ok", () -> {});
		return "not ok";
	}
}

private class SillyClass {
	public function new(d:Dynamic, f:Dynamic) {}
}
