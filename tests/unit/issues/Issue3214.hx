package unit.issues;

private abstract Int64(Int64_t) {
	inline private function new(i) {
		this = i;
	}

	inline public static function make(a:Int, b:Int):Int64 {
		return new Int64(cast a + b);
	}

	@:op(A*B) inline public function mul_i64(a:Int64):Int64 {
		return new Int64(cast this.get() * a.get().get());
	}

	public function get() {
		return this;
	}
}

private abstract Int64_t(Dynamic) {
	public function get() return this;
}

class Issue3214 extends unit.Test {
	function test() {
		var a = Int64.make(1,2);
		var b = Int64.make(3,4);
        var c = a * b;
		eq(21, c.get().get());
	}
}