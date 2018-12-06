package unit.issues;

private class Exception {
	public function new() {}
}

private class Exception2 {
	public function new() {}
}

class Issue7653 extends unit.Test {
	function test() {
		var i;
		try {
			doSomething();
			i = 10;
		}
		catch (e:Exception) {
			i = 20;
		}
		eq(i, 20);
	}

	static function doSomething() {
		try {
			throw new Exception();
		}
		catch (e:Exception2) {}
	}
}