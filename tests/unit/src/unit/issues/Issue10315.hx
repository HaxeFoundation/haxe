package unit.issues;

class Issue10315 extends Test {
	function test() {
		var o = new Obj();
		eq('a', o.call('a'));
		eq('b', o.call(('b':Dynamic)));
	}
}


private class Obj {
	public function new() {}
	public function call(...args:Any) {
		return args[0];
	}
}