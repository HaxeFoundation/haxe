package eval.integers;

/**
	Unsigned 64-bit integer type and operations.
**/
@:coreType abstract UInt64 {
	/** The greates representable UInt64 value */
	extern static public final MAX:UInt64;
	/** The integer `0` */
	extern static public final ZERO:UInt64;
	/** The integer `1` */
	extern static public final ONE:UInt64;

	/**
		Convert the given int value to an unsigned integer.
	**/
	static public function ofInt(i:Int):UInt64;

	/**
		Parse the given string value to an unsigned integer.

		Throws if the given string is not a valid representation of an unsigned
		integer.
	**/
	static public function ofString(s:String):UInt64;

	/**
		Returns the greater of `a` and `b`.
	**/
	static public function max(a:UInt64, b:UInt64):UInt64;

	/**
		Returns the lesser of `a` and `b`.
	**/
	static public function min(a:UInt64, b:UInt64):UInt64;

	/**
		Compare given values.
		Returns `0` if the values are equal.
		Returns negative integer if `a` is lesser than `b`.
		Returns positive integer if `a` is greater than `b`.
	**/
	static public function compare(a:UInt64, b:UInt64):Int;

	/**
		Convert to an integer value.

		The 64-bit unsigned integer is taken modulo 2{^32}, i.e. the top 32 bits
		are lost during the conversion.
	**/
	public function toInt():Int;

	/**
		Return the string representation of its argument.
	**/
	public function toString():String;

	/**
		Successor.
	**/
	public function successor():String;

	/**
		Predecessor.
	**/
	public function predecessor():String;

	/**
		Integer remainder.
		Throws if the divisor is zero.
	**/
	public function remainder(u:UInt64):UInt64;

	@:op(A + B) function add(u:UInt64):UInt64;
	@:op(A - B) function sub(u:UInt64):UInt64;
	@:op(A * B) function mul(u:UInt64):UInt64;
	@:op(A / B) function div(u:UInt64):UInt64;
	@:op(A & B) function logand(u:UInt64):UInt64;
	@:op(A | B) function logor(u:UInt64):UInt64;
	@:op(A ^ B) function logxor(u:UInt64):UInt64;
	@:op(A << B) function shift_left(i:Int):UInt64;
	@:op(A >> B) function shift_right(i:Int):UInt64;
	@:op(~A) function lognot():UInt64;

	@:op(A != B) static inline function eq(a:UInt64, b:UInt64):Bool return compare(a, b) != 0;
	@:op(A == B) static inline function ne(a:UInt64, b:UInt64):Bool return compare(a, b) == 0;
	@:op(A < B) static inline function lt(a:UInt64, b:UInt64):Bool return compare(a, b) < 0;
	@:op(A > B) static inline function gt(a:UInt64, b:UInt64):Bool return compare(a, b) > 0;
	@:op(A <= B) static inline function lte(a:UInt64, b:UInt64):Bool return compare(a, b) <= 0;
	@:op(A >= B) static inline function gte(a:UInt64, b:UInt64):Bool return compare(a, b) >= 0;
}