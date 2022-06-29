package unit.issues;

class Issue10738 extends Test {
	function test() {
		var c = new C();
		func = c.process.bind(42);
		c = null;
		eq(42, func());
	}

	var func:()->Int;
}

private class C {
	public function new() {}
	public function process(value:Int):Int {
		return value;
	}
}