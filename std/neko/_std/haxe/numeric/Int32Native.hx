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
	Neko-specific Int32Native. Neko's Int type is only 31 bits,
	so full 32-bit overflow behavior cannot be guaranteed.
	This provides best-effort semantics within the 31-bit constraint.
	Values near the 32-bit boundaries may not behave identically to
	targets with native 32-bit integers.
**/
typedef Int32Native = Int32NativeImpl;

@:coreApi(check = Off)
private abstract Int32NativeImpl(Int) from Int to Int {
	// Two's complement negation. Best-effort on Neko where values
	// near 32-bit boundaries may overflow the 31-bit Int range.
	public static inline function neg(x:Int32Native):Int32Native
		return cast(~(x : Int) + 1);

	public static inline function add(a:Int32Native, b:Int32Native):Int32Native
		return cast((a : Int) + (b : Int));

	public static inline function sub(a:Int32Native, b:Int32Native):Int32Native
		return cast((a : Int) - (b : Int));

	public static inline function mul(a:Int32Native, b:Int32Native):Int32Native
		return cast((a : Int) * (b : Int));

	public static inline function complement(a:Int32Native):Int32Native
		return cast ~(a : Int);

	public static inline function and(a:Int32Native, b:Int32Native):Int32Native
		return cast((a : Int) & (b : Int));

	public static inline function or(a:Int32Native, b:Int32Native):Int32Native
		return cast((a : Int) | (b : Int));

	public static inline function xor(a:Int32Native, b:Int32Native):Int32Native
		return cast((a : Int) ^ (b : Int));

	public static inline function shl(a:Int32Native, b:Int):Int32Native
		return cast((a : Int) << b);

	public static inline function shr(a:Int32Native, b:Int):Int32Native
		return cast((a : Int) >> b);

	public static inline function ushr(a:Int32Native, b:Int):Int32Native
		return cast((a : Int) >>> b);

	public static function ucompare(a:Int32Native, b:Int32Native):Int {
		if ((a : Int) < 0)
			return (b : Int) < 0 ? (~(b : Int) - ~(a : Int)) : 1;
		return (b : Int) < 0 ? -1 : ((a : Int) - (b : Int));
	}

	public inline function toFloat():Float
		return this;

	public static inline function clamp(x:Int):Int32Native
		return cast x;
}
