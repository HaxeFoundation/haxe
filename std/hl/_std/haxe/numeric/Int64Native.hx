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

#if (hl_ver >= version("1.12.0") && !hl_legacy32)
/**
	HL-native 64-bit integer implementation.
	Shadows the cross-platform emulation with native HL `I64` operations.
	Only available on HL >= 1.12.0 without legacy 32-bit mode.
**/
@:coreApi(check = Off)
abstract Int64Native(hl.I64) from hl.I64 to hl.I64 {
	static var MASK:hl.I64 = {
		var v:hl.I64 = 0xFFFF;
		v | (v << 16);
	}

	public var high(get, set):haxe.Int32;

	inline function get_high():haxe.Int32
		return cast(this >> 32);

	inline function set_high(v:haxe.Int32):haxe.Int32 {
		this = ((cast v : hl.I64) << 32) | (this & MASK);
		return v;
	}

	public var low(get, set):haxe.Int32;

	inline function get_low():haxe.Int32
		return cast this;

	inline function set_low(v:haxe.Int32):haxe.Int32 {
		this = (this & ~MASK) | ((cast v : hl.I64) & MASK);
		return v;
	}

	public inline function new(high:haxe.Int32, low:haxe.Int32) {
		var h:hl.I64 = high;
		var l:hl.I64 = low;
		this = (h << 32) | (l & MASK);
	}

	public static inline function make(high:haxe.Int32, low:haxe.Int32):Int64Native {
		return new Int64Native(high, low);
	}

	public static inline function ofInt(x:Int):Int64Native {
		return cast x;
	}

	public static inline function toInt(x:Int64Native):Int {
		var v:hl.I64 = x;
		if (v < (cast -2147483648 : hl.I64) || v > (cast 2147483647 : hl.I64))
			throw "Overflow";
		return cast v;
	}

	public static inline function isInt64(val:Dynamic):Bool
		return hl.Type.getDynamic(val).kind == HI64;

	public static inline function isNeg(x:Int64Native):Bool
		return (x : hl.I64) < 0;

	public static inline function isZero(x:Int64Native):Bool
		return (x : hl.I64) == 0;

	public static inline function compare(a:Int64Native, b:Int64Native):Int {
		if ((a : hl.I64) < (b : hl.I64))
			return -1;
		if ((a : hl.I64) > (b : hl.I64))
			return 1;
		return 0;
	}

	public static inline function ucompare(a:Int64Native, b:Int64Native):Int {
		if ((a : hl.I64) < 0)
			return ((b : hl.I64) < 0) ? compare(a, b) : 1;
		return ((b : hl.I64) < 0) ? -1 : compare(a, b);
	}

	public static inline function neg(x:Int64Native):Int64Native
		return cast -(x : hl.I64);

	public static inline function add(a:Int64Native, b:Int64Native):Int64Native
		return cast((a : hl.I64) + (b : hl.I64));

	public static inline function sub(a:Int64Native, b:Int64Native):Int64Native
		return cast((a : hl.I64) - (b : hl.I64));

	public static inline function mul(a:Int64Native, b:Int64Native):Int64Native
		return cast((a : hl.I64) * (b : hl.I64));

	public static inline function divMod(dividend:Int64Native, divisor:Int64Native):{quotient:Int64Native, modulus:Int64Native}
		return {quotient: cast((dividend : hl.I64) / (divisor : hl.I64)), modulus: cast((dividend : hl.I64) % (divisor : hl.I64))};

	public static inline function eq(a:Int64Native, b:Int64Native):Bool
		return (a : hl.I64) == (b : hl.I64);

	public static inline function neq(a:Int64Native, b:Int64Native):Bool
		return (a : hl.I64) != (b : hl.I64);

	public static inline function complement(x:Int64Native):Int64Native
		return cast ~(x : hl.I64);

	public static inline function and(a:Int64Native, b:Int64Native):Int64Native
		return cast((a : hl.I64) & (b : hl.I64));

	public static inline function or(a:Int64Native, b:Int64Native):Int64Native
		return cast((a : hl.I64) | (b : hl.I64));

	public static inline function xor(a:Int64Native, b:Int64Native):Int64Native
		return cast((a : hl.I64) ^ (b : hl.I64));

	public static inline function shl(a:Int64Native, b:Int):Int64Native
		return cast((a : hl.I64) << b);

	public static inline function shr(a:Int64Native, b:Int):Int64Native
		return cast((a : hl.I64) >> b);

	public static inline function ushr(a:Int64Native, b:Int):Int64Native
		return cast((a : hl.I64) >>> b);

	public inline function toString():String
		return Std.string(this);

	public static inline function parseString(sParam:String):Int64Native {
		return haxe.numeric.Int64Helper.parseString(sParam);
	}

	public static inline function fromFloat(f:Float):Int64Native {
		return haxe.numeric.Int64Helper.fromFloat(f);
	}
}

#end
