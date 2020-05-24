package unit.issues;

private abstract Lazy<T>(()->T) {
	inline function new(r) this = r;

	@:to public inline function get():T
		return (this)();

	@:from static inline function ofConst<T>(c:T)
		return new Lazy(function () return c);
}

class Issue4798 extends Test {
	public static var INIT: Lazy<String> = 'b';
	function test() {
		eq("b", (INIT: String));
	}
}