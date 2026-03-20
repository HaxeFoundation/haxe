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
import haxe.numeric.Int32Native;

/**
	A cross-platform unsigned 64-bit integer type.

	Built on top of `haxe.numeric.Int64Native`, sharing the same backing
	representation as `haxe.Int64`. All bit-identical operations (addition,
	subtraction, multiplication, bitwise) delegate directly to `Int64Native`.
	Operations that differ for unsigned interpretation (division, comparison,
	right shift, toString) use unsigned-specific implementations provided
	by `Int64Native` (udivMod, utoString, etc.), which native targets can
	override for better performance.
**/
abstract UInt64(Int64Native) from Int64Native to Int64Native {
	private inline function new(x:Int64Native)
		this = x;

	/** The greatest representable UInt64 value: `2^64 - 1`. **/
	public static final MAX:UInt64 = make(0xFFFFFFFF, 0xFFFFFFFF);

	/** The smallest representable UInt64 value: `0`. **/
	public static final MIN:UInt64 = make(0, 0);

	/**
		Makes a copy of `this` UInt64.
	**/
	public inline function copy():UInt64
		return make(high, low);

	/**
		Construct a UInt64 from two 32-bit words `high` and `low`.
	**/
	public static inline function make(high:Int32, low:Int32):UInt64
		return new UInt64(Int64Native.make(high, low));

	/**
		Returns a UInt64 with the value of the Int `x`.
		`x` is sign-extended to fill 64 bits (same bit pattern as `Int64.fromInt`).
	**/
	@:from public static inline function fromInt(x:Int):UInt64
		return new UInt64(Int64Native.ofInt(x));

	/**
		Returns a UInt64 with the value of the Int `x`.
		`x` is sign-extended to fill 64 bits (same bit pattern as `Int64.fromInt`).
	**/
	@:deprecated("Use fromInt instead")
	public static inline function ofInt(x:Int):UInt64
		return fromInt(x);

	/**
		Returns the low 32 bits of `x` as an Int.
		The top 32 bits are discarded.
	**/
	public static inline function toInt(x:UInt64):Int
		return x.low.toInt();

	/**
		Compares `a` and `b` as unsigned 64-bit integers.
		Returns a negative value if `a < b`, positive if `a > b`,
		or 0 if `a == b`.
	**/
	public static inline function compare(a:UInt64, b:UInt64):Int
		return Int64Native.ucompare(a, b);

	/**
		Compares `a` and `b` as unsigned 64-bit integers.
		Returns a negative value if `a < b`, positive if `a > b`,
		or 0 if `a == b`.
	**/
	public static inline function ucompare(a:UInt64, b:UInt64):Int
		return Int64Native.ucompare(a, b);

	/**
		Returns `true` if `x` is exactly zero.
	**/
	public static inline function isZero(x:UInt64):Bool
		return Int64Native.isZero(x);

	/**
		Returns an unsigned decimal `String` representation of `x`.
	**/
	public inline function toString():String
		return Int64Native.utoString(this);

	/**
		Parses an unsigned decimal string into a UInt64.
		Throws on invalid input, negative values, or overflow.
	**/
	public static inline function parseString(sParam:String):UInt64 {
		return Int64Native.uparseString(sParam);
	}

	/**
		Converts a non-negative Float to UInt64.
		Throws on negative, NaN, Infinite, or values exceeding `2^53-1`.
	**/
	public static inline function fromFloat(f:Float):UInt64 {
		return Int64Native.ufromFloat(f);
	}

	/**
		Converts this unsigned 64-bit value to Float.
		Values above `2^53` may lose precision.
	**/
	public inline function toFloat():Float {
		return Int64Native.utoFloat(this);
	}

	/**
		Returns the low 32 bits of this UInt64 as a UInt32.
		The high 32 bits are discarded.
	**/
	public inline function toUInt32():UInt32
		return cast Int64Native.toInt(this);

	/**
		Implicit conversion to Int64 (same bit pattern, same size).
	**/
	@:to private inline function toInt64():Int64
		return cast this;

	/**
		Performs unsigned integer division of `dividend` by `divisor`.
		Returns `{ quotient : UInt64, modulus : UInt64 }`.
	**/
	private static function divMod(dividend:UInt64, divisor:UInt64):{quotient:UInt64, modulus:UInt64} {
		var r = Int64Native.udivMod(dividend, divisor);
		return {quotient: r.quotient, modulus: r.modulus};
	}

	/**
		Returns the two's complement negation of `x`.
	**/
	@:op(-A) public static inline function neg(x:UInt64):UInt64
		return Int64Native.neg(x);

	@:op(++A) private inline function preIncrement():UInt64 {
		this = Int64Native.add(this, Int64Native.ofInt(1));
		return this;
	}

	@:op(A++) private inline function postIncrement():UInt64 {
		var ret = this;
		this = Int64Native.add(this, Int64Native.ofInt(1));
		return ret;
	}

	@:op(--A) private inline function preDecrement():UInt64 {
		this = Int64Native.sub(this, Int64Native.ofInt(1));
		return this;
	}

	@:op(A--) private inline function postDecrement():UInt64 {
		var ret = this;
		this = Int64Native.sub(this, Int64Native.ofInt(1));
		return ret;
	}

	/**
		Returns the sum of `a` and `b`.
	**/
	@:op(A + B) public static inline function add(a:UInt64, b:UInt64):UInt64
		return Int64Native.add(a, b);

	/**
		Returns `a` minus `b`.
	**/
	@:op(A - B) public static inline function sub(a:UInt64, b:UInt64):UInt64
		return Int64Native.sub(a, b);

	/**
		Returns the product of `a` and `b`.
	**/
	@:op(A * B) public static inline function mul(a:UInt64, b:UInt64):UInt64
		return Int64Native.mul(a, b);

	/**
		Returns the unsigned quotient of `a` divided by `b`.
	**/
	@:op(A / B) public static inline function div(a:UInt64, b:UInt64):UInt64
		return Int64Native.udivMod(a, b).quotient;

	/**
		Returns the unsigned modulus of `a` divided by `b`.
	**/
	@:op(A % B) public static inline function mod(a:UInt64, b:UInt64):UInt64
		return Int64Native.udivMod(a, b).modulus;

	/**
		Returns `true` if `a` is equal to `b`.
	**/
	@:op(A == B) public static inline function eq(a:UInt64, b:UInt64):Bool
		return Int64Native.eq(a, b);

	/**
		Returns `true` if `a` is not equal to `b`.
	**/
	@:op(A != B) public static inline function neq(a:UInt64, b:UInt64):Bool
		return Int64Native.neq(a, b);

	@:op(A < B) private static inline function lt(a:UInt64, b:UInt64):Bool
		return compare(a, b) < 0;

	@:op(A <= B) private static inline function lte(a:UInt64, b:UInt64):Bool
		return compare(a, b) <= 0;

	@:op(A > B) private static inline function gt(a:UInt64, b:UInt64):Bool
		return compare(a, b) > 0;

	@:op(A >= B) private static inline function gte(a:UInt64, b:UInt64):Bool
		return compare(a, b) >= 0;

	/**
		Returns the bitwise NOT of `a`.
	**/
	@:op(~A) private static inline function complement(a:UInt64):UInt64
		return Int64Native.complement(a);

	/**
		Returns the bitwise AND of `a` and `b`.
	**/
	@:op(A & B) public static inline function and(a:UInt64, b:UInt64):UInt64
		return Int64Native.and(a, b);

	/**
		Returns the bitwise OR of `a` and `b`.
	**/
	@:op(A | B) public static inline function or(a:UInt64, b:UInt64):UInt64
		return Int64Native.or(a, b);

	/**
		Returns the bitwise XOR of `a` and `b`.
	**/
	@:op(A ^ B) public static inline function xor(a:UInt64, b:UInt64):UInt64
		return Int64Native.xor(a, b);

	/**
		Returns `a` left-shifted by `b` bits.
	**/
	@:op(A << B) public static inline function shl(a:UInt64, b:Int):UInt64
		return Int64Native.shl(a, b);

	/**
		Returns `a` right-shifted by `b` bits with zero-extension (logical shift).
		For unsigned types, both `>>` and `>>>` perform logical (unsigned) right shift.
	**/
	@:op(A >> B) public static inline function shr(a:UInt64, b:Int):UInt64
		return Int64Native.ushr(a, b);

	/**
		Returns `a` right-shifted by `b` bits with zero-extension (logical shift).
	**/
	@:op(A >>> B) public static inline function ushr(a:UInt64, b:Int):UInt64
		return Int64Native.ushr(a, b);

	public var high(get, never):Int32;

	private inline function get_high()
		return this.high;

	public var low(get, never):Int32;

	private inline function get_low()
		return this.low;
}
