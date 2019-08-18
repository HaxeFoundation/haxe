package unit.issues;

private abstract CsStack<T>(Int) {
	public function new(i) this = i;
	public function get() return this;
}

class Issue6752 extends unit.Test {
	function test() {
		var f = CsStack.new;
		eq(1, f(1).get());
	}
}