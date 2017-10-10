package unit.issues;

private class C {
	public final v:Int;
	function new(v:Int) this.v = v;
	static function f(v:Int) return v;

	public static inline function g(?v = 0) return new C(v);
	public static inline function g2(?v = 0) return f(v);
}

class Issue4968 extends Test {
	function test() {
		eq(C.g().v, 0);
		eq(C.g2(), 0);
	}
}
