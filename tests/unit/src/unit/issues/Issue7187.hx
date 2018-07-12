package unit.issues;

class Issue7187 extends unit.Test {
	function test() {
		eq(5, func(function(x:Int) return x));
	}

	public static inline function func(f:Int->Int) {
		whatever(f != null);
		return f(5);
	}

	@:pure(false) static function whatever(x) { }
}