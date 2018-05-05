package unit.issues;

private class C {
	public var f:Int;
	public function new() {
		f = 1;
	}
}

class Issue5508 extends unit.Test {
	var i = 2;
	static var c = new C();

	function test() {
		eq(~mask(i), -5);
		eq(nPure(c).f, 1);
	}

	@:pure
	inline static function mask(i:Int):Int {
		return 0x1 << i;
	}

    @:pure
    static inline function nPure(a:C) {
        return if (a == null) a else a;
    }
}
