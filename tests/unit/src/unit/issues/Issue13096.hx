package unit.issues;

private class Box {
	public var v:Int;

	public inline function new(v)
		this.v = v;
}

private abstract A(Box) {
	public inline function new(v)
		this = new Box(v);

	public static inline function twice(x:A)
		return new A(x.get() * 2);

	inline function get()
		return this.v;

	public function toString()
		return "" + this.v;
}

class Issue13096 extends Test {
	static var calls = 0;

	static function next()
		return new A(++calls);

	function test() {
		eq("1", Std.string(next()));
		eq(1, calls);
		eq("4", Std.string(A.twice(new A(2))));
		eq(new Map<String, Int>().toString(), Std.string(new Map<String, Int>()));
	}
}
