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

import haxe.numeric.Int32Native;

/**
	A cross-platform unsigned 32-bit integer type.

	Built on top of `haxe.numeric.Int32Native`, sharing the same backing
	representation as `haxe.Int32`. Arithmetic and bitwise operations are
	identical at the bit level; operations that differ for unsigned interpretation
	(comparison, division, modulo, right shift, `toString`) use unsigned-specific
	implementations.

	On targets where `Int` is natively 32-bit (C++, JVM, HashLink), no masking
	overhead is incurred. On scripting targets, values are masked to 32 bits after
	each operation that may overflow.
**/
@:forward.ops
@:transitive
abstract UInt32(Int32Native) from Int32Native to Int32Native {
	private inline function new(x:Int32Native)
		this = x;

	/** The greatest representable UInt32 value: `2^32 - 1` (= `4294967295`). **/
	public static final MAX:UInt32 = cast 0xFFFFFFFF;

	/** The smallest representable UInt32 value: `0`. **/
	public static final MIN:UInt32 = cast 0;

	@:op(++A) private inline function preIncrement():UInt32 {
		this = Int32Native.add(this, 1);
		return cast this;
	}

	@:op(A++) private inline function postIncrement():UInt32 {
		var ret = this;
		this = Int32Native.add(this, 1);
		return ret;
	}

	@:op(--A) private inline function preDecrement():UInt32 {
		this = Int32Native.sub(this, 1);
		return cast this;
	}

	@:op(A--) private inline function postDecrement():UInt32 {
		var ret = this;
		this = Int32Native.sub(this, 1);
		return ret;
	}

	@:op(A / B) private static inline function div(a:UInt32, b:UInt32):UInt32
		return Int32Native.udivMod(a, b).quotient;

	@:op(A % B) private static inline function mod(a:UInt32, b:UInt32):UInt32
		return Int32Native.udivMod(a, b).modulus;

	@:op(A == B) @:commutative private static inline function equalsInt<T:Int>(a:UInt32, b:T):Bool
		return (a : Int) == b;

	@:op(A != B) @:commutative private static inline function notEqualsInt<T:Int>(a:UInt32, b:T):Bool
		return (a : Int) != b;

	@:op(A == B) @:commutative private static inline function equalsFloat<T:Float>(a:UInt32, b:T):Bool
		return (a : Float) == b;

	@:op(A != B) @:commutative private static inline function notEqualsFloat<T:Float>(a:UInt32, b:T):Bool
		return (a : Float) != b;

	@:op(A < B) private static inline function lt(a:UInt32, b:UInt32):Bool
		return compare(a, b) < 0;

	@:op(A <= B) private static inline function lte(a:UInt32, b:UInt32):Bool
		return compare(a, b) <= 0;

	@:op(A > B) private static inline function gt(a:UInt32, b:UInt32):Bool
		return compare(a, b) > 0;

	@:op(A >= B) private static inline function gte(a:UInt32, b:UInt32):Bool
		return compare(a, b) >= 0;

	@:op(A < B) private static inline function ltFloat<T:Float>(a:UInt32, b:T):Bool
		return (a : Float) < b;

	@:op(A < B) private static inline function floatLt<T:Float>(a:T, b:UInt32):Bool
		return a < (b:Float);

	@:op(A <= B) private static inline function lteFloat<T:Float>(a:UInt32, b:T):Bool
		return (a : Float) <= b;

	@:op(A <= B) private static inline function floatLte<T:Float>(a:T, b:UInt32):Bool
		return a <= (b : Float);

	@:op(A > B) private static inline function gtFloat<T:Float>(a:UInt32, b:T):Bool
		return (a : Float) > b;

	@:op(A > B) private static inline function floatGt<T:Float>(a:T, b:UInt32):Bool
		return a > (b : Float);

	@:op(A >= B) private static inline function gteFloat<T:Float>(a:UInt32, b:T):Bool
		return (a : Float) >= b;

	@:op(A >= B) private static inline function floatGte<T:Float>(a:T, b:UInt32):Bool
		return a >= (b : Float);

	@:op(A >> B) private static inline function shr(a:UInt32, b:Int):UInt32
		return Int32Native.ushr(a, b);

	@:to public inline function toFloat():Float
		return Int32Native.utoFloat(this);

	/**
		Compare `a` and `b` in signed mode.
		Returns a negative value if `a < b`, positive if `a > b`, or 0 if `a == b`.
	**/
	public static inline function compare(a:UInt32, b:UInt32):Int
		return Int32Native.ucompare(a, b);

	/**
		Compare `a` and `b` in unsigned mode.
	**/
	public static inline function ucompare(a:UInt32, b:UInt32):Int
		return Int32Native.ucompare(a, b);

	/**
		Returns `true` if `x` is zero (i.e. the minimum value).
	**/
	public static inline function isZero(x:UInt32):Bool {
		var n:Int32Native = x;
		return (n : Int) == 0;
	}

	/**
		Returns a UInt32 with the value of the Int `x`.
		Only the low 32 bits of `x` are used (masking applied if necessary).
	**/
	@:from public static inline function fromInt(x:Int):UInt32
		return new UInt32(Int32Native.clamp(x));

	/**
		Parses an unsigned decimal string into a `UInt32`.
		Throws `NumberFormatError` on invalid input or out-of-range values.
	**/
	public static inline function parseString(sParam:String):UInt32
		return new UInt32(Int32Native.uparseString(sParam));

	// Extra

	/** @deprecated Use `fromInt` instead. **/
	@:deprecated("Use fromInt instead")
	public static inline function ofInt(x:Int):UInt32
		return fromInt(x);

	/**
		Returns the value of this UInt32 as an Int (same bit pattern,
		may appear negative for values ≥ `2^31`).
	**/
	public static inline function toInt(x:UInt32):Int {
		var n:Int32Native = x;
		return (n : Int);
	}

	/**
		Returns an unsigned decimal String representation of `x`.
	**/
	public inline function toString():String
		return Int32Native.utoString(this);
}
