package unit.issues;

private abstract A(Array<Int>) {
	public function new() {
		this = [];
	}

	public inline function add(i : Int) : Void {
		this.push(i);
	}

	public function get() {
		return this.pop();
	}
}

class Issue2767 extends Test {
	function test() {
		var a = new A();
		var f = a.add;
		f(12);
		eq(12, a.get());
	}
}