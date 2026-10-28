package jvm;

@:notNull @:runtimeValue @:coreType extern abstract Int64 from Int from Float {
	@:op(A + B) public static function add(lhs:Int64, rhs:Int64):Int64;

	@:op(A * B) public static function mul(lhs:Int64, rhs:Int64):Int64;

	@:op(A % B) public static function mod(lhs:Int64, rhs:Int64):Int64;

	@:op(A - B) public static function sub(lhs:Int64, rhs:Int64):Int64;

	@:op(A / B) public static function div(lhs:Int64, rhs:Int64):Int64;

	@:op(A | B) public static function or(lhs:Int64, rhs:Int64):Int64;

	@:op(A ^ B) public static function xor(lhs:Int64, rhs:Int64):Int64;

	@:op(A & B) public static function and(lhs:Int64, rhs:Int64):Int64;

	@:op(A << B) public static function shl(lhs:Int64, rhs:Int64):Int64;

	@:op(A >> B) public static function shr(lhs:Int64, rhs:Int64):Int64;

	@:op(A >>> B) public static function ushr(lhs:Int64, rhs:Int64):Int64;

	@:op(A > B) public static function gt(lhs:Int64, rhs:Int64):Bool;

	@:op(A >= B) public static function gte(lhs:Int64, rhs:Int64):Bool;

	@:op(A < B) public static function lt(lhs:Int64, rhs:Int64):Bool;

	@:op(A <= B) public static function lte(lhs:Int64, rhs:Int64):Bool;

	@:op(~A) public static function bneg(t:Int64):Int64;

	@:op(-A) public static function neg(t:Int64):Int64;

	@:op(++A) public static function preIncrement(t:Int64):Int64;

	@:op(A++) public static function postIncrement(t:Int64):Int64;

	@:op(--A) public static function preDecrement(t:Int64):Int64;

	@:op(A--) public static function postDecrement(t:Int64):Int64;
}
