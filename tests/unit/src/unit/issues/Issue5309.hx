package unit.issues;

class Issue5309 extends unit.Test {
	function test() {
	  eq(25, f2b(0.1));
	}

	public static inline function clamp( f : Float, min = 0., max = 1. ) {
		return f < min ? min : f > max ? max : f;
	}

	public static function f2b( v:Float ) : Int {
		return Std.int(clamp(v) * 255.0);
	}
}