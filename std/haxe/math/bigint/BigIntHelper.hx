/*
 * Copyright (C)2005-2022 Haxe Foundation
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
 
package haxe.math.bigint;

/* Original code courtesy Chuck Batson (github.com/cbatson) */
class BigIntHelper
{

	/**
		"Numbler of leading zeros" - return the number of leading
		0-value bits in the binary representation of `x`.
	**/
	public static function nlz(x : Int) : Int
	{
		// From "Hacker's Delight", Second Edition; Henry S. Warren, Jr.; 2013. Figure 5-15, p. 102.
		var y : Int, m : Int, n : Int;

		y = -(x >>> 16);
		m = (y >> 16) & 16;
		n = 16 - m;
		x = x >>> m;

		y = x - 0x100;
		m = (y >> 16) & 8;
		n = n + m;
		x = x << m;

		y = x - 0x1000;
		m = (y >> 16) & 4;
		n = n + m;
		x = x << m;

		y = x - 0x4000;
		m = (y >> 16) & 2;
		n = n + m;
		x = x << m;

		y = x >> 14;
		m = y & (~y >> 1);
		return n + 2 - m;
	}


	/**
		"Ceiling power of two" -- round up to the least power of two
		greater than or equal to input `x`, which is interpreted as
		unsigned.
	**/
	public static function clp2(x : Int32) : Int
	{
		// From "Hacker's Delight", Second Edition; Henry S. Warren, Jr.; 2013. Figure 3-3, p. 62.
		x = x - 1;
		x = x | (x >> 1);
		x = x | (x >> 2);
		x = x | (x >> 4);
		x = x | (x >> 8);
		x = x | (x >> 16);
		return x + 1;
	}

	/**
		Unsigned greater than comparison.

		Returns `true` if `a > b` when both `a` and `b` are
		interpreted as unsigned integers; `false` otherwise.
	**/
	public static inline function u32gtu32(a : Int, b : Int) : Bool
	{
		return (a ^ -2147483648) > (b ^ -2147483648);		// unsigned comparison, see "Hacker's Delight" p. 25.
	}


	/**
		Integer division of unsigned 32-bit integer by unsigned 16-bit integer.

		Result is undefined when `divisor` <= 0 or `divisor` >= 2^16.
	**/
	public static function u32divu16(dividend : Int32, divisor : Int32) : Int
	{
		/*
			Complicated because Haxe's division is always performed as
			floating-point.  Here we rely on the ability to exactly represent
			a 31-bit integer as a Float.  In other words, 64-bit floating
			point is required.

			TODO: Implement a method without this restriction.
			TODO: Consider C++-specific optimization here.
		*/
		// From "Hacker's Delight", Second Edition; Henry S. Warren, Jr.; 2013. Section 9-3, p. 192.
		var t : Int = divisor >> 31;
		var nprime : Int = dividend & ~t;
		var q : Int32 = Std.int((nprime >>> 1) / divisor) << 1;
		var r : Int = dividend - q * divisor;
		var c : Int = u32geu32(r, divisor) ? 1 : 0;
		return q + c;
	}

	/**
		Unsigned greater than or equal comparison.
		Returns `true` if `a >= b` when both `a` and `b` are
		interpreted as unsigned integers; `false` otherwise.
	**/
	public static inline function u32geu32(a : Int, b : Int) : Bool
	{
			return (a ^ -2147483648) >= (b ^ -2147483648);		// unsigned comparison, see "Hacker's Delight" p. 25.
	}
	
	/**
		Number of trailing zeros - return the number of trailing
		0-value bits 
	**/
	public static function ntz( x : Int32 ):Int
	{
		if (x == 0) return 32;
		var y:Int;
		var n:Int = 31;
		y = x << 16; if (y != 0) { n -= 16; x = y; }
		y = x <<  8; if (y != 0) { n -=  8; x = y; }
		y = x <<  4; if (y != 0) { n -=  4; x = y; }
		y = x <<  2; if (y != 0) { n -=  2; x = y; }
		return   (n - ((x << 1) >>> 31));
	}

}
