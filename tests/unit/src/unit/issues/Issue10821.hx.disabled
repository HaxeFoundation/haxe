package unit.issues;

class Issue10821 extends Test {
	function test() {
		eq('foo', Std.string(new Dummy()));
	}
}

private class Dummy {
	var toString:()->String;

	public function new() {
		toString = () -> 'foo';
	}
}