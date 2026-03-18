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
abstract UInt32(Int32Native) from Int32Native to Int32Native {
	private inline function new(x:Int32Native)
		this = x;

	/** The greatest representable UInt32 value: `2^32 - 1` (= `4294967295`). **/
	public static final MAX:UInt32 = cast 0xFFFFFFFF;

	/** The smallest representable UInt32 value: `0`. **/
	public static final MIN:UInt32 = cast 0;

	/**
		Returns this UInt32 value as a Float, interpreting the bits as unsigned.
		Values with the high bit set are in the range `2147483648`–`4294967295`.
	**/
	@:to public inline function toFloat():Float
		return Int32Native.utoFloat(this);

	/**
		Returns a UInt32 with the value of the Int `x`.
		Only the low 32 bits of `x` are used (masking applied if necessary).
	**/
	@:from public static inline function ofInt(x:Int):UInt32
		return new UInt32(Int32Native.clamp(x));

	/**
		Returns the value of this UInt32 as an Int (same bit pattern,
		may appear negative for values ≥ `2^31`).
	**/
	public static inline function toInt(x:UInt32):Int {
		var n:Int32Native = x;
		return (n : Int);
	}

	@:to private inline function toIntInternal():Int {
		return (this : Int);
	}

	/**
		Returns `true` if `x` is exactly zero.
	**/
	public static inline function isZero(x:UInt32):Bool {
		var n:Int32Native = x;
		return (n : Int) == 0;
	}

	/**
		Compares `a` and `b` as unsigned 32-bit integers.
		Returns a negative value if `a < b`, positive if `a > b`, or 0 if `a == b`.
	**/
	public static inline function compare(a:UInt32, b:UInt32):Int
		return Int32Native.ucompare(a, b);

	/**
		Returns an unsigned decimal String representation of `x`.
	**/
	public inline function toString():String
		return Int32Native.utoString(this);

	/**
		Performs unsigned integer division of `dividend` by `divisor`.
		Returns `{ quotient : UInt32, modulus : UInt32 }`.
		Throws on division by zero.
	**/
	public static function divMod(dividend:UInt32, divisor:UInt32):{quotient:UInt32, modulus:UInt32} {
		var r = Int32Native.udivMod(dividend, divisor);
		return {quotient: r.quotient, modulus: r.modulus};
	}

	/**
		Reinterprets the bits of an `Int32` as a `UInt32`.
	**/
	public static inline function fromInt32(x:Int32):UInt32
		return new UInt32((x : Int32Native));

	/**
		Reinterprets the bits of this `UInt32` as a signed `Int32`.
	**/
	public inline function toInt32():Int32
		return (this : Int32Native);

	@:op(-A) public static inline function neg(x:UInt32):UInt32
		return Int32Native.neg(x);

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

	/**
		Returns the sum of `a` and `b` (wraps on overflow).
	**/
	@:op(A + B) public static inline function add(a:UInt32, b:UInt32):UInt32
		return Int32Native.add(a, b);

	@:op(A + B) @:commutative private static inline function addInt(a:UInt32, b:Int):UInt32
		return add(a, b);

	@:op(A + B) @:commutative private static inline function addFloat(a:UInt32, b:Float):Float
		return (a : Float) + b;

	/**
		Returns `a` minus `b` (wraps on underflow).
	**/
	@:op(A - B) public static inline function sub(a:UInt32, b:UInt32):UInt32
		return Int32Native.sub(a, b);

	@:op(A - B) private static inline function subInt(a:UInt32, b:Int):UInt32
		return sub(a, b);

	@:op(A - B) private static inline function intSub(a:Int, b:UInt32):UInt32
		return sub(a, b);

	@:op(A - B) private static inline function subFloat(a:UInt32, b:Float):Float
		return (a : Float) - b;

	@:op(A - B) private static inline function floatSub(a:Float, b:UInt32):Float
		return a - (b : Float);

	/**
		Returns the product of `a` and `b` (wraps on overflow).
	**/
	@:op(A * B) public static inline function mul(a:UInt32, b:UInt32):UInt32
		return Int32Native.mul(a, b);

	@:op(A * B) @:commutative private static inline function mulInt(a:UInt32, b:Int):UInt32
		return mul(a, b);

	@:op(A * B) @:commutative private static inline function mulFloat(a:UInt32, b:Float):Float
		return (a : Float) * b;

	/**
		Returns the unsigned quotient of `a` divided by `b`.
		Throws on division by zero.
	**/
	@:op(A / B) public static inline function div(a:UInt32, b:UInt32):UInt32
		return Int32Native.udivMod(a, b).quotient;

	@:op(A / B) private static inline function divInt(a:UInt32, b:Int):UInt32
		return div(a, b);

	@:op(A / B) private static inline function intDiv(a:Int, b:UInt32):UInt32
		return div(a, b);

	@:op(A / B) private static inline function divFloat(a:UInt32, b:Float):Float
		return (a : Float) / b;

	@:op(A / B) private static inline function floatDiv(a:Float, b:UInt32):Float
		return a / (b : Float);

	/**
		Returns the unsigned modulus of `a` divided by `b`.
		Throws on division by zero.
	**/
	@:op(A % B) public static inline function mod(a:UInt32, b:UInt32):UInt32
		return Int32Native.udivMod(a, b).modulus;

	@:op(A % B) private static inline function modInt(a:UInt32, b:Int):UInt32
		return mod(a, b);

	@:op(A % B) private static inline function intMod(a:Int, b:UInt32):UInt32
		return mod(a, b);

	@:op(A % B) private static inline function modFloat(a:UInt32, b:Float):Float
		return (a : Float) % b;

	@:op(A % B) private static inline function floatMod(a:Float, b:UInt32):Float
		return a % (b : Float);

	/**
		Returns `true` if `a` is equal to `b`.
	**/
	@:op(A == B) public static inline function eq(a:UInt32, b:UInt32):Bool
		return (a : Int32Native) == (b : Int32Native);

	@:op(A == B) @:commutative private static inline function eqInt<T:Int>(a:UInt32, b:T):Bool
		return (a : Int32Native) == b;

	@:op(A == B) @:commutative private static inline function equalsFloat<T:Float>(a:UInt32, b:T):Bool
		return (a : Float) == b;

	/**
		Returns `true` if `a` is not equal to `b`.
	**/
	@:op(A != B) public static inline function neq(a:UInt32, b:UInt32):Bool
		return (a : Int32Native) != (b : Int32Native);

	@:op(A != B) @:commutative private static inline function neqInt<T:Int>(a:UInt32, b:T):Bool
		return (a : Int32Native) != b;

	@:op(A != B) @:commutative private static inline function notEqualsFloat<T:Float>(a:UInt32, b:T):Bool
		return (a : Float) != b;

	@:op(A < B) private static inline function lt(a:UInt32, b:UInt32):Bool
		return compare(a, b) < 0;

	@:op(A < B) private static inline function ltInt(a:UInt32, b:Int):Bool
		return lt(a, b);

	@:op(A < B) private static inline function intLt(a:Int, b:UInt32):Bool
		return lt(a, b);

	@:op(A <= B) private static inline function lte(a:UInt32, b:UInt32):Bool
		return compare(a, b) <= 0;

	@:op(A <= B) private static inline function lteInt(a:UInt32, b:Int):Bool
		return lte(a, b);

	@:op(A <= B) private static inline function intLte(a:Int, b:UInt32):Bool
		return lte(a, b);

	@:op(A > B) private static inline function gt(a:UInt32, b:UInt32):Bool
		return compare(a, b) > 0;

	@:op(A > B) private static inline function gtInt(a:UInt32, b:Int):Bool
		return gt(a, b);

	@:op(A > B) private static inline function intGt(a:Int, b:UInt32):Bool
		return gt(a, b);

	@:op(A >= B) private static inline function gte(a:UInt32, b:UInt32):Bool
		return compare(a, b) >= 0;

	@:op(A >= B) private static inline function gteInt(a:UInt32, b:Int):Bool
		return gte(a, b);

	@:op(A >= B) private static inline function intGte(a:Int, b:UInt32):Bool
		return gte(a, b);

	@:op(A < B) private static inline function ltFloat<T:Float>(a:UInt32, b:T):Bool
		return (a : Float) < b;

	@:op(A < B) private static inline function floatLt<T:Float>(a:T, b:UInt32):Bool
		return a < (b : Float);

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

	/**
		Returns the bitwise NOT of `a`.
	**/
	@:op(~A) public static inline function complement(a:UInt32):UInt32
		return Int32Native.complement(a);

	/**
		Returns the bitwise AND of `a` and `b`.
	**/
	@:op(A & B) public static inline function and(a:UInt32, b:UInt32):UInt32
		return Int32Native.and(a, b);

	/**
		Returns the bitwise OR of `a` and `b`.
	**/
	@:op(A | B) public static inline function or(a:UInt32, b:UInt32):UInt32
		return Int32Native.or(a, b);

	/**
		Returns the bitwise XOR of `a` and `b`.
	**/
	@:op(A ^ B) public static inline function xor(a:UInt32, b:UInt32):UInt32
		return Int32Native.xor(a, b);

	/**
		Returns `a` left-shifted by `b` bits (wraps on overflow).
	**/
	@:op(A << B) public static inline function shl(a:UInt32, b:Int):UInt32
		return Int32Native.shl(a, b);

	/**
		Returns `a` right-shifted by `b` bits with zero-extension (logical shift).
		Unlike signed `Int32`, this always performs an unsigned (logical) shift.
	**/
	@:op(A >> B) public static inline function shr(a:UInt32, b:Int):UInt32
		return Int32Native.ushr(a, b);

	/**
		Returns `a` right-shifted by `b` bits with zero-extension (logical shift).
	**/
	@:op(A >>> B) public static inline function ushr(a:UInt32, b:Int):UInt32
		return Int32Native.ushr(a, b);
}
