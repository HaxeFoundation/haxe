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

package haxe.numeric;

/**
	Shared Int32Native implementation for targets where `Int` is natively
	32-bit (C++, JVM, HashLink). No masking is needed; operations are identity
	casts on the underlying type.

	This type is used internally via `typedef Int32Native = Int32Direct`
	in the target-specific overrides.
**/
abstract Int32Direct(Int) from Int to Int {
	public static inline function neg(x:Int32Direct):Int32Direct
		// Use ~x+1 (two's complement) rather than unary minus.
		// On CPPIA, unary minus on Int can return a value wider than 32 bits,
		// while bitwise NOT and addition stay within the native 32-bit int range.
		return cast(~(x : Int) + 1);

	public static inline function add(a:Int32Direct, b:Int32Direct):Int32Direct
		return cast((a : Int) + (b : Int));

	public static inline function sub(a:Int32Direct, b:Int32Direct):Int32Direct
		return cast((a : Int) - (b : Int));

	public static inline function mul(a:Int32Direct, b:Int32Direct):Int32Direct
		return cast((a : Int) * (b : Int));

	public static inline function complement(a:Int32Direct):Int32Direct
		return cast ~(a : Int);

	public static inline function and(a:Int32Direct, b:Int32Direct):Int32Direct
		return cast((a : Int) & (b : Int));

	public static inline function or(a:Int32Direct, b:Int32Direct):Int32Direct
		return cast((a : Int) | (b : Int));

	public static inline function xor(a:Int32Direct, b:Int32Direct):Int32Direct
		return cast((a : Int) ^ (b : Int));

	public static inline function shl(a:Int32Direct, b:Int):Int32Direct
		return cast((a : Int) << b);

	public static inline function shr(a:Int32Direct, b:Int):Int32Direct
		return cast((a : Int) >> b);

	public static inline function ushr(a:Int32Direct, b:Int):Int32Direct
		return cast((a : Int) >>> b);

	public static inline function compare(a:Int32Direct, b:Int32Direct):Int {
		var av:Int = a;
		var bv:Int = b;
		return av < bv ? -1 : (av > bv ? 1 : 0);
	}

	public static function ucompare(a:Int32Direct, b:Int32Direct):Int {
		if ((a : Int) < 0)
			return (b : Int) < 0 ? (~(b : Int) - ~(a : Int)) : 1;
		return (b : Int) < 0 ? -1 : ((a : Int) - (b : Int));
	}

	public inline function toFloat():Float
		return this;

	public static inline function div(a:Int32Direct, b:Int32Direct):Int32Direct
		return cast(Std.int((a : Int) / (b : Int)));

	public static inline function mod(a:Int32Direct, b:Int32Direct):Int32Direct
		return cast((a : Int) % (b : Int));

	/**
		Convert `a` to Float treating its bit-pattern as an unsigned 32-bit value.
	**/
	public static inline function utoFloat(a:Int32Direct):Float
		return Int32Helper.utoFloat(a);

	/**
		Perform unsigned integer division/modulo on `a` and `b`.
		Throws on division by zero.
	**/
	public static inline function udivMod(a:Int32Direct, b:Int32Direct):{quotient:Int32Direct, modulus:Int32Direct} {
		var r = Int32Helper.udivMod(a, b);
		return {quotient: cast r.quotient, modulus: cast r.modulus};
	}

	/**
		Returns the unsigned decimal string representation of `a`.
	**/
	public static inline function utoString(a:Int32Direct):String
		#if jvm
		return java.lang.Integer.IntegerClass.toUnsignedString(a);
		#else
		return Std.string(utoFloat(a));
		#end

	/**
		Parse a signed decimal string into an `Int32Direct`.
		Throws `NumberFormatError` on invalid input or out-of-range values.
	**/
	public static inline function parseString(s:String):Int32Direct
		return cast Int32Helper.parseString(s);

	/**
		Parse an unsigned decimal string into an `Int32Direct`.
		Values ≥ 2^31 are stored as negative (two's complement bit-pattern).
		Throws `NumberFormatError` on invalid input or out-of-range values.
	**/
	public static inline function uparseString(s:String):Int32Direct
		return cast Int32Helper.uparseString(s);

	public static inline function clamp(x:Int):Int32Direct
		return cast x;
}
