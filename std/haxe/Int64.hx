/*
 * Copyright (C)2005-2019 Haxe Foundation
 *
 * Permission is hereby granted, free of charge, to any person obtaining a
 * copy of this software and associated documentation files (the "Software"),
 * to deal in the Software without restriction, including without limitation
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,
 * and/or sell copies of the Software, and to permit persons to whom the
 * Software is furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
 * DEALINGS IN THE SOFTWARE.
 */

package haxe;

import haxe.numeric.Int64Native;

/**
	A cross-platform signed 64-bit integer.
	Int64 instances can be created from two 32-bit words using `Int64.make()`.

	This abstract defines the operator overloads and public API surface.
	The actual implementation is in `haxe.numeric.Int64Native`, which can be
	shadowed by platform-specific `_std` directories for native support.
**/
#if flash
@:notNull
#end
@:transitive
abstract Int64(Int64Native) from Int64Native to Int64Native {
	private inline function new(x:Int64Native)
		this = x;

	/** The greatest representable Int64 value: `2^63 - 1`. **/
	public static final MAX:Int64 = make(0x7FFFFFFF, 0xFFFFFFFF);

	/** The smallest representable Int64 value: `-2^63`. **/
	public static final MIN:Int64 = make(0x80000000, 0);

	/**
		Makes a copy of `this` Int64.
	**/
	public inline function copy():Int64
		return make(high, low);

	/**
		Construct an Int64 from two 32-bit words `high` and `low`.
	**/
	public static inline function make(high:Int32, low:Int32):Int64
		return new Int64(Int64Native.make(high, low));

	/**
		Returns an Int64 with the value of the Int `x`.
		`x` is sign-extended to fill 64 bits.
	**/
	@:from public static inline function fromInt(x:Int):Int64
		return new Int64(Int64Native.ofInt(x));

	/**
		Returns an Int64 with the value of the Int `x`.
		`x` is sign-extended to fill 64 bits.
	**/
	@:deprecated("Use fromInt instead")
	public static inline function ofInt(x:Int):Int64
		return fromInt(x);

	/**
		Returns an Int with the low 32 bits of the Int64 `x`.
		The high 32 bits are discarded.
	**/
	public static inline function toInt(x:Int64):Int
		return Int64Native.toInt(x);

	/**
		Compares `a` and `b` in signed mode.
		Returns a negative value if `a < b`, positive if `a > b`,
		or 0 if `a == b`.
	**/
	public static inline function compare(a:Int64, b:Int64):Int
		return Int64Native.compare(a, b);

	/**
		Compares `a` and `b` in unsigned mode.
		Returns a negative value if `a < b`, positive if `a > b`,
		or 0 if `a == b`.
	**/
	public static inline function ucompare(a:Int64, b:Int64):Int
		return Int64Native.ucompare(a, b);

	/**
		Returns `true` if `x` is less than zero.
	**/
	public static inline function isNeg(x:Int64):Bool
		return Int64Native.isNeg(x);

	/**
		Returns `true` if `x` is exactly zero.
	**/
	public static inline function isZero(x:Int64):Bool
		return Int64Native.isZero(x);

	public inline function toString():String
		return this.toString();

	public static inline function parseString(sParam:String):Int64 {
		return Int64Native.parseString(sParam);
	}

	public static inline function fromFloat(f:Float):Int64 {
		return Int64Native.fromFloat(f);
	}

	/**
		Converts this Int64 to a Float.
		Values between -2^53 and 2^53 are exact; larger values may lose precision.
	**/
	public inline function toFloat():Float {
		return Int64Native.toFloat(this);
	}

	/**
		Performs signed integer division of `dividend` by `divisor`.
		Returns `{ quotient : Int64, modulus : Int64 }`.
	**/
	private static function divMod(dividend:Int64, divisor:Int64):{quotient:Int64, modulus:Int64} {
		var r = Int64Native.divMod(dividend, divisor);
		return {quotient: r.quotient, modulus: r.modulus};
	}

	/**
		Returns the negative of `x`.
	**/
	@:op(-A) public static inline function neg(x:Int64):Int64
		return Int64Native.neg(x);

	@:op(++A) private inline function preIncrement():Int64 {
		this = Int64Native.add(this, Int64Native.ofInt(1));
		return cast this;
	}

	@:op(A++) private inline function postIncrement():Int64 {
		var ret = this;
		preIncrement();
		return ret;
	}

	@:op(--A) private inline function preDecrement():Int64 {
		this = Int64Native.sub(this, Int64Native.ofInt(1));
		return cast this;
	}

	@:op(A--) private inline function postDecrement():Int64 {
		var ret = this;
		preDecrement();
		return ret;
	}

	/**
		Returns the sum of `a` and `b`.
	**/
	@:op(A + B) public static inline function add(a:Int64, b:Int64):Int64
		return Int64Native.add(a, b);

	/**
		Returns `a` minus `b`.
	**/
	@:op(A - B) public static inline function sub(a:Int64, b:Int64):Int64
		return Int64Native.sub(a, b);

	/**
		Returns the product of `a` and `b`.
	**/
	@:op(A * B) public static inline function mul(a:Int64, b:Int64):Int64
		return Int64Native.mul(a, b);

	/**
		Returns the quotient of `a` divided by `b`.
	**/
	@:op(A / B) public static inline function div(a:Int64, b:Int64):Int64
		return divMod(a, b).quotient;

	/**
		Returns the modulus of `a` divided by `b`.
	**/
	@:op(A % B) public static inline function mod(a:Int64, b:Int64):Int64
		return divMod(a, b).modulus;

	/**
		Returns `true` if `a` is equal to `b`.
	**/
	@:op(A == B) public static inline function eq(a:Int64, b:Int64):Bool
		return Int64Native.eq(a, b);

	/**
		Returns `true` if `a` is not equal to `b`.
	**/
	@:op(A != B) public static inline function neq(a:Int64, b:Int64):Bool
		return Int64Native.neq(a, b);

	@:op(A < B) private static inline function lt(a:Int64, b:Int64):Bool
		return compare(a, b) < 0;

	@:op(A <= B) private static inline function lte(a:Int64, b:Int64):Bool
		return compare(a, b) <= 0;

	@:op(A > B) private static inline function gt(a:Int64, b:Int64):Bool
		return compare(a, b) > 0;

	@:op(A >= B) private static inline function gte(a:Int64, b:Int64):Bool
		return compare(a, b) >= 0;

	/**
		Returns the bitwise NOT of `a`.
	**/
	@:op(~A) private static inline function complement(a:Int64):Int64
		return Int64Native.complement(a);

	/**
		Returns the bitwise AND of `a` and `b`.
	**/
	@:op(A & B) public static inline function and(a:Int64, b:Int64):Int64
		return Int64Native.and(a, b);

	/**
		Returns the bitwise OR of `a` and `b`.
	**/
	@:op(A | B) public static inline function or(a:Int64, b:Int64):Int64
		return Int64Native.or(a, b);

	/**
		Returns the bitwise XOR of `a` and `b`.
	**/
	@:op(A ^ B) public static inline function xor(a:Int64, b:Int64):Int64
		return Int64Native.xor(a, b);

	/**
		Returns `a` left-shifted by `b` bits.
	**/
	@:op(A << B) public static inline function shl(a:Int64, b:Int):Int64
		return Int64Native.shl(a, b);

	/**
		Returns `a` right-shifted by `b` bits in signed mode.
		`a` is sign-extended.
	**/
	@:op(A >> B) public static inline function shr(a:Int64, b:Int):Int64
		return Int64Native.shr(a, b);

	/**
		Returns `a` right-shifted by `b` bits in unsigned mode.
		`a` is padded with zeroes.
	**/
	@:op(A >>> B) public static inline function ushr(a:Int64, b:Int):Int64
		return Int64Native.ushr(a, b);

	public var high(get, never):Int32;

	private inline function get_high()
		return this.high;

	public var low(get, never):Int32;

	private inline function get_low()
		return this.low;

	// Used by platform-specific FPHelper on Lua/Python/PHP via @:privateAccess.
	// Not available on C++ because the C++ Int64NativeImpl has read-only high/low.
	#if !cpp
	private inline function set_high(x)
		return this.high = x;

	private inline function set_low(x)
		return this.low = x;
	#end

	// Extra

	/**
		Returns whether the value `val` is of type `haxe.Int64`
	**/
	inline public static function isInt64(val:Dynamic):Bool
		return Int64Native.isInt64(val);

	/**
		Returns a signed decimal `String` representation of `x`.
	**/
	@:deprecated("Use toString instead")
	public static inline function toStr(x:Int64):String
		return x.toString();
}
