package unit.issues;

private abstract Meters(Float) from Float {
	inline public static var One:Meters = 1;
	inline public static var Zero:Meters = 0;

	extern inline public function new(f:Float) {
		this = f;
	}

	@:op(-A) static private function neg(s:Meters):Meters;
	@:op(A+B) static private function add(lhs:Meters, rhs:Meters):Meters;
	@:op(A-B) static private function sub(lhs:Meters, rhs:Meters):Meters;
	@:op(A>B) static private function gt(lhs:Meters, rhs:Meters):Bool;
	@:op(A>=B) static private function gte(lhs:Meters, rhs:Meters):Bool;
	@:op(A<B) static private function lt(lhs:Meters, rhs:Meters):Bool;
	@:op(A<=B) static private function lte(lhs:Meters, rhs:Meters):Bool;
	@:op(A==B) static private function eq(lhs:Meters, rhs:Meters):Bool;
	extern inline public function float() return this;

	@:to inline public function toString() {
		return '$this(m)';
	}
}

class Issue3345 extends Test {
	#if !lua
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
	#end
}
