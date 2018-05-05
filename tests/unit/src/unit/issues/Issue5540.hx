package unit.issues;

class Issue5540 extends unit.Test {
	function test() {
		eq(1, get().propertySet -= 1);
	}

	static function get() return { propertySet:new Signal(2) };
}

private abstract Signal(Int) {
	public function new(i) this = i;

	@:op(A -= B)
	public function opRemove(i:Int) return this - i;
}