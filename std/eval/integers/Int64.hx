package eval.integers;

import haxe.Int32;

/**
	Signed 64-bit integer type and operations.
**/
@:runtimeValue
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

	static public function make(high:Int32, low:Int32):Int64;

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

	public function toInt32():Int32;

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
	public function successor():Int64;

	/**
		Predecessor.
	**/
	public function predecessor():Int64;

	/**
		Integer remainder.
		Throws if the divisor is zero.
	**/
	public function remainder(u:Int64):Int64;

	public function add(u:Int64):Int64;
	public function sub(u:Int64):Int64;
	public function mul(u:Int64):Int64;
	public function div(u:Int64):Int64;
	public function logand(u:Int64):Int64;
	public function logor(u:Int64):Int64;
	public function logxor(u:Int64):Int64;
	public function shift_left(i:Int):Int64;
	public function shift_right(i:Int):Int64;
	public function shift_right_logical(i:Int):Int64;
	public function lognot():Int64;
	@:op(-A) public function neg():Int64;

	@:op(++A) function preIncr():Int64;
	@:op(A++) function postIncr():Int64;
	@:op(--A) function preDecr():Int64;
	@:op(A--) function postDecr():Int64;
}