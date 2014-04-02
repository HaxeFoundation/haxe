package unit.issues;
import unit.Test;

private abstract MyAbstract(Int) {
	public inline function new() {
		this = 1;
	}

	public function test() {
		var f = function() return get();
		return f;
	}

	function get() return this;
}

class Issue2823 extends Test {
	function test() {
		var a = new MyAbstract();
		eq(1, a.test()());
	}
}