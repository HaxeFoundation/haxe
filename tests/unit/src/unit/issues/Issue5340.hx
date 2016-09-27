package unit.issues;

private class Vector{
	public var x : Float = 0;

	public var inverse(get, never) : Vector;

	public inline function new(?x : Float = 0.) {
		this.x = x;
	}
	inline function get_inverse() return new Vector(-x);
	public function toString() return 'Vec($x)';
}

private class Matrix {
	public var m : Array< Float > = [0.11, 0.33];

	public function new() {}
	public inline function back() return new Vector(m[0]);
	public inline function fwd() return back().inverse;
}

class Issue5340 extends unit.Test {
	function test() {
		var m = new Matrix();
		m.m[0] = 0.22;
		var dir = m.fwd();

		eq("Vec(-0.22)", "" + dir);
	}
}