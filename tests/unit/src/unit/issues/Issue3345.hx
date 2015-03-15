package unit.issues;

private abstract Meters(Float) from Float {
	inline public static var One:Meters = 1;
	inline public static var Zero:Meters = 0;

	@:extern inline public function new(f:Float) {
		this = f;
	}

	@:extern @:op(-A) public static function neg(s:Meters):Meters;
	@:extern @:op(A+B) public static function add(lhs:Meters, rhs:Meters):Meters;
	@:extern @:op(A-B) public static function sub(lhs:Meters, rhs:Meters):Meters;
	@:extern @:op(A>B) public static function gt(lhs:Meters, rhs:Meters):Bool;
	@:extern @:op(A>=B) public static function gte(lhs:Meters, rhs:Meters):Bool;
	@:extern @:op(A<B) public static function lt(lhs:Meters, rhs:Meters):Bool;
	@:extern @:op(A<=B) public static function lte(lhs:Meters, rhs:Meters):Bool;
	@:extern @:op(A==B) public static function eq(lhs:Meters, rhs:Meters):Bool;
	@:extern inline public function float() return this;

	@:to inline public function toString() {
		return '$this(m)';
	}
}

class Issue3345 extends Test {
	function test() {
		var acc:Meters = .0;
		for (i in 0...10)
			acc += 10;
		eq("100(m)", acc);

		var acc:Meters = .0;
		for (i in 0...10)
			acc -= 10;
		eq("-100(m)", acc);
	}
}