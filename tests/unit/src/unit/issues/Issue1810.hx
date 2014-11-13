package unit.issues;

private abstract A(T) to T {
	public inline function new(x:T) this = x;
	public function get() return this;
}

private abstract T(X) from X to X {
	public inline function new(x:X) this = x;
	@:from public static inline function fromArr(x:Array<Int>) return new T(new X(x[0]));
	public function get() return this;
}

private class X {
	public var i:Int;
	public function new(i:Int) {
		this.i = i;
	}
}

class Issue1810 extends Test {
	function test() {
		var a = new A([12]);
		//eq(12, a.get().get().i);
	}
}