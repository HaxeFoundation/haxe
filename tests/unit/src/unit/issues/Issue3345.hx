package unit.issues;

private abstract Meters(Float) from Float {
	inline public static var One:Meters = 1;
	inline public static var Zero:Meters = 0;

	extern inline public function new(f:Float) {
		this = f;
	}

	@:op(-A) extern public static function neg(s:Meters):Meters;
	@:op(A+B) extern public static function add(lhs:Meters, rhs:Meters):Meters;
	@:op(A-B) extern public static function sub(lhs:Meters, rhs:Meters):Meters;
	@:op(A>B) extern public static function gt(lhs:Meters, rhs:Meters):Bool;
	@:op(A>=B) extern public static function gte(lhs:Meters, rhs:Meters):Bool;
	@:op(A<B) extern public static function lt(lhs:Meters, rhs:Meters):Bool;
	@:op(A<=B) extern public static function lte(lhs:Meters, rhs:Meters):Bool;
	@:op(A==B) extern public static function eq(lhs:Meters, rhs:Meters):Bool;
	extern inline public function float() return this;

	@:to inline public function toString() {
		return '$this(m)';
	}
}

class Issue3345 extends Test {
	function test() {
		var acc:Meters = .0;
		for (i in 0...10)
			acc += 10;
#if !lua
		eq("100(m)", acc);
#end

		var acc:Meters = .0;
		for (i in 0...10)
			acc -= 10;
#if !lua
		eq("-100(m)", acc);
#end
	}
}
