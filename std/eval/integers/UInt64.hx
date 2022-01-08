package eval.integers;

/**
	Unsigned 64-bit integer type and operations.
**/
@:coreType abstract UInt64 {
	/** The greatest representable UInt64 value. */
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
		Convert to a signed integer value.
	**/
	public function toInt64():Int64;

	/**
		Return the string representation of this value.
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

	function add(u:UInt64):UInt64;
	function sub(u:UInt64):UInt64;
	function mul(u:UInt64):UInt64;
	function div(u:UInt64):UInt64;
	function logand(u:UInt64):UInt64;
	function logor(u:UInt64):UInt64;
	function logxor(u:UInt64):UInt64;
	function shift_left(i:Int):UInt64;
	function shift_right(i:Int):UInt64;
	function lognot():UInt64;

	@:op(A + B) inline function _add(u:UInt64):UInt64 return this.add(u);
	@:op(A - B) inline function _sub(u:UInt64):UInt64 return this.sub(u);
	@:op(A * B) inline function _mul(u:UInt64):UInt64 return this.mul(u);
	@:op(A / B) inline function _div(u:UInt64):UInt64 return this.div(u);
	@:op(A % B) inline function _mod(u:UInt64):UInt64 return this.remainder(u);
	@:op(A & B) inline function _logand(u:UInt64):UInt64 return this.logand(u);
	@:op(A | B) inline function _logor(u:UInt64):UInt64 return this.logor(u);
	@:op(A ^ B) inline function _logxor(u:UInt64):UInt64 return this.logxor(u);
	@:op(A << B) inline function _shift_left(i:Int):UInt64 return this.shift_left(i);
	@:op(A >> B) inline function _shift_right(i:Int):UInt64 return this.shift_right(i);
	@:op(~A) inline function _lognot():UInt64 return this.lognot();

	@:op(A != B) static inline function eq(a:UInt64, b:UInt64):Bool return compare(a, b) != 0;
	@:op(A == B) static inline function ne(a:UInt64, b:UInt64):Bool return compare(a, b) == 0;
	@:op(A < B) static inline function lt(a:UInt64, b:UInt64):Bool return compare(a, b) < 0;
	@:op(A > B) static inline function gt(a:UInt64, b:UInt64):Bool return compare(a, b) > 0;
	@:op(A <= B) static inline function lte(a:UInt64, b:UInt64):Bool return compare(a, b) <= 0;
	@:op(A >= B) static inline function gte(a:UInt64, b:UInt64):Bool return compare(a, b) >= 0;
}