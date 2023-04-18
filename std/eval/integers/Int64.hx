package eval.integers;

/**
	Signed 64-bit integer type and operations.
**/
@:coreType abstract Int64 {
	/** The greatest representable Int64 value. */
	extern static public final MAX:Int64;
	/** The smallest representable Int64 value. */
	extern static public final MIN:Int64;
	/** The integer `0` */
	extern static public final ZERO:Int64;
	/** The integer `1` */
	extern static public final ONE:Int64;

	/**
		Convert the given int value to Int64.
	**/
	static public function ofInt(i:Int):Int64;

	/**
		Parse the given string value to Int64.
		Throws if the given string is not a valid representation of Int64.
	**/
	static public function ofString(s:String):Int64;

	/**
		Convert `haxe.Int64` to `eval.integers.Int64`
	**/
	@:from static public function ofHxInt64(hx:haxe.Int64):Int64;

	/**
		Returns the greater of `a` and `b`.
	**/
	static public function max(a:Int64, b:Int64):Int64;

	/**
		Returns the lesser of `a` and `b`.
	**/
	static public function min(a:Int64, b:Int64):Int64;

	/**
		Compare given values.
		Returns `0` if the values are equal.
		Returns negative integer if `a` is lesser than `b`.
		Returns positive integer if `a` is greater than `b`.
	**/
	static public function compare(a:Int64, b:Int64):Int;

	/**
		Convert to an integer value.
		The 64-bit signed integer is taken modulo 2{^32}, i.e. the top 32 bits
		are lost during the conversion.
	**/
	public function toInt():Int;

	/**
		Convert to an unsigned integer value.
	**/
	public function toUInt64():UInt64;

	/**
		Convert to `haxe.Int64`.
	**/
	@:to public function toHxInt64():haxe.Int64;

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
	public function remainder(u:Int64):Int64;

	function add(u:Int64):Int64;
	function sub(u:Int64):Int64;
	function mul(u:Int64):Int64;
	function div(u:Int64):Int64;
	function logand(u:Int64):Int64;
	function logor(u:Int64):Int64;
	function logxor(u:Int64):Int64;
	function shift_left(i:Int):Int64;
	function shift_right(i:Int):Int64;
	function lognot():Int64;

	@:op(A + B) inline function _add(u:Int64):Int64 return this.add(u);
	@:op(A - B) inline function _sub(u:Int64):Int64 return this.sub(u);
	@:op(A * B) inline function _mul(u:Int64):Int64 return this.mul(u);
	@:op(A / B) inline function _div(u:Int64):Int64 return this.div(u);
	@:op(A % B) inline function _mod(u:Int64):Int64 return this.remainder(u);
	@:op(A & B) inline function _logand(u:Int64):Int64 return this.logand(u);
	@:op(A | B) inline function _logor(u:Int64):Int64 return this.logor(u);
	@:op(A ^ B) inline function _logxor(u:Int64):Int64 return this.logxor(u);
	@:op(A << B) inline function _shift_left(i:Int):Int64 return this.shift_left(i);
	@:op(A >> B) inline function _shift_right(i:Int):Int64 return this.shift_right(i);
	@:op(~A) inline function _lognot():Int64 return this.lognot();

	@:op(A != B) static inline function eq(a:Int64, b:Int64):Bool return compare(a, b) != 0;
	@:op(A == B) static inline function ne(a:Int64, b:Int64):Bool return compare(a, b) == 0;
	@:op(A < B) static inline function lt(a:Int64, b:Int64):Bool return compare(a, b) < 0;
	@:op(A > B) static inline function gt(a:Int64, b:Int64):Bool return compare(a, b) > 0;
	@:op(A <= B) static inline function lte(a:Int64, b:Int64):Bool return compare(a, b) <= 0;
	@:op(A >= B) static inline function gte(a:Int64, b:Int64):Bool return compare(a, b) >= 0;
}