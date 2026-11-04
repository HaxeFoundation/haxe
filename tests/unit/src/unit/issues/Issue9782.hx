package unit.issues;

private interface I {
	var x(get, set):Int;
}

private class C implements I {
	public var x(get, set):Int;

	public function new() {}

	function get_x()
		return 12;

	function set_x(v:Int)
		return v;
}

private abstract A(I) {
	public var x(get, set):Int;

	public function new(i:I)
		this = i;

	public extern overload inline function get_x():Int
		return this.x;

	public extern overload inline function get_x(i:Int):Int
		return i;

	function set_x(v:Int):Int
		return v;
}

class Issue9782 extends Test {
	function test() {
		var a = new A(new C());
		eq(12, a.x);
		eq(5, a.get_x(5));
	}
}
