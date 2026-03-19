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
	A cross-platform signed 32-bit integer with consistent overflow behavior.

	This abstract defines the operator overloads and public API surface.
	The actual implementation is in `haxe.numeric.Int32Native`, which can be
	shadowed by platform-specific `_std` directories for native support.

	On targets with native 32-bit Int (C++, JVM, HL), Int32 maps directly
	to the platform Int with no overhead. On scripting targets (JS, PHP, Python,
	Lua), operations are clamped to 32-bit range after each computation. On Neko,
	values exceeding 31-bit range are auto-promoted to Float by the VM while
	preserving correct 32-bit arithmetic.
**/
@:transitive
abstract Int32(Int32Native) from Int32Native to Int32Native {
	private inline function new(x:Int32Native)
		this = x;

	/** The greatest representable Int32 value: `2^31 - 1`. **/
	public static final MAX:Int32 = 0x7FFFFFFF;

	/** The smallest representable Int32 value: `-2^31`. **/
	public static final MIN:Int32 = 0x80000000;

	@:op(-A) private static inline function neg(x:Int32):Int32
		return Int32Native.neg(x);

	@:op(++A) private inline function preIncrement():Int32 {
		this = Int32Native.add(this, 1);
		return cast this;
	}

	@:op(A++) private inline function postIncrement():Int32 {
		var ret = this;
		this = Int32Native.add(this, 1);
		return ret;
	}

	@:op(--A) private inline function preDecrement():Int32 {
		this = Int32Native.sub(this, 1);
		return cast this;
	}

	@:op(A--) private inline function postDecrement():Int32 {
		var ret = this;
		this = Int32Native.sub(this, 1);
		return ret;
	}

	@:op(A + B) private static inline function add(a:Int32, b:Int32):Int32
		return Int32Native.add(a, b);

	@:op(A - B) private static inline function sub(a:Int32, b:Int32):Int32
		return Int32Native.sub(a, b);

	@:op(A * B) private static inline function mul(a:Int32, b:Int32):Int32
		return Int32Native.mul(a, b);

	@:op(A / B) private static inline function div(a:Int32, b:Int32):Int32
		return Int32Native.div(a, b);

	@:op(A % B) private static inline function mod(a:Int32, b:Int32):Int32
		return Int32Native.mod(a, b);

	@:op(A < B) private static inline function lt(a:Int32, b:Int32):Bool
		return compare(a, b) < 0;

	@:op(A <= B) private static inline function lte(a:Int32, b:Int32):Bool
		return compare(a, b) <= 0;

	@:op(A > B) private static inline function gt(a:Int32, b:Int32):Bool
		return compare(a, b) > 0;

	@:op(A >= B) private static inline function gte(a:Int32, b:Int32):Bool
		return compare(a, b) >= 0;

	@:op(A < B) private static inline function ltFloat<T:Float>(a:Int32, b:T):Bool
		return (a : Float) < b;

	@:op(A < B) private static inline function floatLt<T:Float>(a:T, b:Int32):Bool
		return a < (b : Float);

	@:op(A <= B) private static inline function lteFloat<T:Float>(a:Int32, b:T):Bool
		return (a : Float) <= b;

	@:op(A <= B) private static inline function floatLte<T:Float>(a:T, b:Int32):Bool
		return a <= (b : Float);

	@:op(A > B) private static inline function gtFloat<T:Float>(a:Int32, b:T):Bool
		return (a : Float) > b;

	@:op(A > B) private static inline function floatGt<T:Float>(a:T, b:Int32):Bool
		return a > (b : Float);

	@:op(A >= B) private static inline function gteFloat<T:Float>(a:Int32, b:T):Bool
		return (a : Float) >= b;

	@:op(A >= B) private static inline function floatGte<T:Float>(a:T, b:Int32):Bool
		return a >= (b : Float);

	@:op(~A) private static inline function complement(a:Int32):Int32
		return Int32Native.complement(a);

	@:op(A & B) private static inline function and(a:Int32, b:Int32):Int32
		return Int32Native.and(a, b);

	@:op(A | B) private static inline function or(a:Int32, b:Int32):Int32
		return Int32Native.or(a, b);

	@:op(A ^ B) private static inline function xor(a:Int32, b:Int32):Int32
		return Int32Native.xor(a, b);

	@:op(A << B) private static inline function shl(a:Int32, b:Int):Int32
		return Int32Native.shl(a, b);

	@:op(A >> B) private static inline function shr(a:Int32, b:Int):Int32
		return Int32Native.shr(a, b);

	@:op(A >>> B) private static inline function ushr(a:Int32, b:Int):Int32
		return Int32Native.ushr(a, b);

	@:to public inline function toFloat():Float
		return (this : Int);

	/**
		Compare `a` and `b` in signed mode.
		Returns a negative value if `a < b`, positive if `a > b`, or 0 if `a == b`.
	**/
	public static inline function compare(a:Int32, b:Int32):Int
		return Int32Native.compare(a, b);

	/**
		Compare `a` and `b` in unsigned mode.
	**/
	public static inline function ucompare(a:Int32, b:Int32):Int
		return Int32Native.ucompare(a, b);

	/**
		Returns `true` if `x` is less than zero.
	**/
	public static inline function isNeg(x:Int32):Bool
		return (x : Int) < 0;

	/**
		Returns `true` if `x` is exactly zero.
	**/
	public static inline function isZero(x:Int32):Bool
		return (x : Int) == 0;

	/**
		Returns an `Int32` with the value of the `Int` `x`.
		Only the low 32 bits of `x` are used (masking applied if necessary).
	**/
	@:from public static inline function fromInt(x:Int):Int32
		return Int32Native.clamp(x);

	/**
		Parses a signed decimal string into an `Int32`.
		Throws `NumberFormatError` on invalid input or out-of-range values.
	**/
	public static inline function parseString(sParam:String):Int32
		return Int32Native.parseString(sParam);
}
