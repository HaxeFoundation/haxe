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

import haxe.numeric.Int64Data;

/**
	Cross-platform emulation of a 64-bit integer using two 32-bit words.
	This is the default implementation used on targets without native 64-bit
	integer support. Targets with native support can shadow this file via
	their `_std` directory to provide an optimized version.
**/
abstract Int64Native(Int64Data) from Int64Data to Int64Data {
	public var high(get, set):haxe.Int32;

	inline function get_high():haxe.Int32
		return this.high;

	inline function set_high(v:haxe.Int32):haxe.Int32
		return this.high = v;

	public var low(get, set):haxe.Int32;

	inline function get_low():haxe.Int32
		return this.low;

	inline function set_low(v:haxe.Int32):haxe.Int32
		return this.low = v;

	public inline function new(high:haxe.Int32, low:haxe.Int32) {
		this = new Int64Data(high, low);
	}

	public static inline function make(high:haxe.Int32, low:haxe.Int32):Int64Native {
		return new Int64Native(high, low);
	}

	public static inline function ofInt(x:Int):Int64Native {
		#if lua
		return make((x : haxe.Int32) >> 31, (x : haxe.Int32));
		#else
		return make(x >> 31, x);
		#end
	}

	public static inline function toInt(x:Int64Native):Int {
		if (x.high != x.low >> 31)
			throw "Overflow";

		return x.low;
	}

	public static inline function isInt64(val:Dynamic):Bool {
		return Std.isOfType(val, Int64Data);
	}

	public static inline function isNeg(x:Int64Native):Bool {
		return x.high < 0;
	}

	public static inline function isZero(x:Int64Native):Bool {
		return x.high == 0 && x.low == 0;
	}

	public static inline function compare(a:Int64Native, b:Int64Native):Int {
		var v = a.high - b.high;
		v = if (v != 0) v else haxe.Int32.ucompare(a.low, b.low);
		return a.high < 0 ? (b.high < 0 ? v : -1) : (b.high >= 0 ? v : 1);
	}

	public static inline function ucompare(a:Int64Native, b:Int64Native):Int {
		var v = haxe.Int32.ucompare(a.high, b.high);
		return if (v != 0) v else haxe.Int32.ucompare(a.low, b.low);
	}

	public static inline function neg(x:Int64Native):Int64Native {
		var high = ~x.high;
		var low = -x.low;
		if (low == 0)
			high++;
		return make(high, low);
	}

	public static inline function add(a:Int64Native, b:Int64Native):Int64Native {
		var high = a.high + b.high;
		var low = a.low + b.low;
		if (haxe.Int32.ucompare(low, a.low) < 0)
			high++;
		return make(high, low);
	}

	public static inline function sub(a:Int64Native, b:Int64Native):Int64Native {
		var high = a.high - b.high;
		var low = a.low - b.low;
		if (haxe.Int32.ucompare(a.low, b.low) < 0)
			high--;
		return make(high, low);
	}

	@:pure(false)
	public static #if !lua inline #end function mul(a:Int64Native, b:Int64Native):Int64Native {
		var mask = 0xFFFF;
		var al = a.low & mask, ah = a.low >>> 16;
		var bl = b.low & mask, bh = b.low >>> 16;
		var p00 = al * bl;
		var p10 = ah * bl;
		var p01 = al * bh;
		var p11 = ah * bh;
		var low = p00;
		var high = p11 + (p01 >>> 16) + (p10 >>> 16);
		p01 <<= 16;
		low += p01;
		if (haxe.Int32.ucompare(low, p01) < 0)
			high++;
		p10 <<= 16;
		low += p10;
		if (haxe.Int32.ucompare(low, p10) < 0)
			high++;
		high += a.low * b.high + a.high * b.low;
		return make(high, low);
	}

	public static function divMod(dividend:Int64Native, divisor:Int64Native):{quotient:Int64Native, modulus:Int64Native} {
		// Handle special cases of 0 and 1
		if (divisor.high == 0) {
			switch (divisor.low) {
				case 0:
					throw "divide by zero";
				case 1:
					return {quotient: make(dividend.high, dividend.low), modulus: ofInt(0)};
			}
		}

		var divSign = isNeg(dividend) != isNeg(divisor);

		var modulus = isNeg(dividend) ? neg(dividend) : make(dividend.high, dividend.low);
		divisor = isNeg(divisor) ? neg(divisor) : divisor;

		var quotient = ofInt(0);
		var mask = ofInt(1);

		while (!isNeg(divisor)) {
			var cmp = ucompare(divisor, modulus);
			divisor = shl(divisor, 1);
			mask = shl(mask, 1);
			if (cmp >= 0)
				break;
		}

		while (!isZero(mask)) {
			if (ucompare(modulus, divisor) >= 0) {
				quotient = or(quotient, mask);
				modulus = sub(modulus, divisor);
			}
			mask = ushr(mask, 1);
			divisor = ushr(divisor, 1);
		}

		if (divSign)
			quotient = neg(quotient);
		if (isNeg(dividend))
			modulus = neg(modulus);

		return {
			quotient: quotient,
			modulus: modulus
		};
	}

	public static inline function eq(a:Int64Native, b:Int64Native):Bool {
		return a.high == b.high && a.low == b.low;
	}

	public static inline function neq(a:Int64Native, b:Int64Native):Bool {
		return a.high != b.high || a.low != b.low;
	}

	public static inline function complement(a:Int64Native):Int64Native {
		return make(~a.high, ~a.low);
	}

	public static inline function and(a:Int64Native, b:Int64Native):Int64Native {
		return make(a.high & b.high, a.low & b.low);
	}

	public static inline function or(a:Int64Native, b:Int64Native):Int64Native {
		return make(a.high | b.high, a.low | b.low);
	}

	public static inline function xor(a:Int64Native, b:Int64Native):Int64Native {
		return make(a.high ^ b.high, a.low ^ b.low);
	}

	public static inline function shl(a:Int64Native, b:Int):Int64Native {
		b &= 63;
		return if (b == 0) make(a.high, a.low) else if (b < 32) make((a.high << b) | (a.low >>> (32 - b)), a.low << b) else make(a.low << (b - 32), 0);
	}

	public static inline function shr(a:Int64Native, b:Int):Int64Native {
		b &= 63;
		return if (b == 0) make(a.high, a.low) else if (b < 32) make(a.high >> b, (a.high << (32 - b)) | (a.low >>> b)); else make(a.high >> 31, a.high >> (b - 32));
	}

	public static inline function ushr(a:Int64Native, b:Int):Int64Native {
		b &= 63;
		return if (b == 0) make(a.high, a.low) else if (b < 32) make(a.high >>> b, (a.high << (32 - b)) | (a.low >>> b)); else make(0, clamp(a.high >>> (b - 32)));
	}

	#if php
	static var extraBits:Int = php.Const.PHP_INT_SIZE * 8 - 32;
	#end

	#if !lua
	inline
	#end
	static function clamp(x:Int):Int {
		// force to-int conversion on platforms that require it
		#if js
		return x | 0;
		#elseif php
		// we might be on 64-bit php, so sign extend from 32-bit
		return (x << extraBits) >> extraBits;
		#elseif python
		return (python.Syntax.code("{0} % {1}", (x + python.Syntax.opPow(2, 31)), python.Syntax.opPow(2, 32)) : Int) - python.Syntax.opPow(2, 31);
		#elseif lua
		return lua.Boot.clampInt32(x);
		#else
		return x;
		#end
	}

	/**
		Returns a signed decimal `String` representation of the value.
	**/
	public inline function toString():String
		return this.toString();

	public static inline function parseString(sParam:String):Int64Native {
		return haxe.numeric.Int64Helper.parseString(sParam);
	}

	public static inline function fromFloat(f:Float):Int64Native {
		return haxe.numeric.Int64Helper.fromFloat(f);
	}
}
