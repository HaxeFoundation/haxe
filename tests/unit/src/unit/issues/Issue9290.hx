package unit.issues;

class Issue9290 extends unit.Test {
	static var a:A = 1;
	static var b:Array<A> = [1];

	function test() {
		a.x += 2;
		eq(3, a.x);

		b[0].x += 3;
		eq(4, b[0].x);

		var d = new Dummy();
		d.a.x += 5;
		eq(6, d.a.x);
	}
}

private class Dummy {
	public var a:A = 1;
	public function new() {}
}

private abstract A(Int) from Int {
	public var x(get,set):Int;
	function get_x() return this;
	inline function set_x(value) return this = value;
}