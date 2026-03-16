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

@:dox(hide)
@:coreApi(check = Off)
abstract Int64Native(jvm.Int64) from jvm.Int64 to jvm.Int64 {
	public var high(get, set):haxe.Int32;

	inline function get_high():haxe.Int32
		return cast(this >> 32);

	inline function set_high(v:haxe.Int32):haxe.Int32 {
		this = ((cast v : jvm.Int64) << 32) | (this & lowMask());
		return v;
	}

	public var low(get, set):haxe.Int32;

	inline function get_low():haxe.Int32
		return cast this;

	inline function set_low(v:haxe.Int32):haxe.Int32 {
		this = (this & highMask()) | ((cast v : jvm.Int64) & lowMask());
		return v;
	}

	/** 0x00000000FFFFFFFFL **/
	static inline function lowMask():jvm.Int64 {
		return ((cast 0 : jvm.Int64) | (cast -1 : jvm.Int64)) >>> 32;
	}

	/** 0xFFFFFFFF00000000L **/
	static inline function highMask():jvm.Int64 {
		return ~lowMask();
	}

	public inline function new(high:haxe.Int32, low:haxe.Int32) {
		this = ((cast high : jvm.Int64) << 32) | ((cast low : jvm.Int64) & lowMask());
	}

	public static inline function make(high:haxe.Int32, low:haxe.Int32):Int64Native {
		return new Int64Native(high, low);
	}

	public static inline function ofInt(x:Int):Int64Native {
		return cast x;
	}

	public static inline function toInt(x:Int64Native):Int {
		var v:jvm.Int64 = x;
		if (v < ((cast -2147483648 : jvm.Int64))
			|| v > ((cast 2147483647 : jvm.Int64)))
			throw "Overflow";
		return cast v;
	}

	public static inline function isInt64(val:Dynamic):Bool
		return Std.isOfType(val, java.lang.Long.LongClass);

	public static inline function isNeg(x:Int64Native):Bool
		return (x : jvm.Int64) < 0;

	public static inline function isZero(x:Int64Native):Bool
		return (x : jvm.Int64) == 0;

	public static inline function compare(a:Int64Native, b:Int64Native):Int {
		if ((a : jvm.Int64) < (b : jvm.Int64))
			return -1;
		if ((a : jvm.Int64) > (b : jvm.Int64))
			return 1;
		return 0;
	}

	public static inline function ucompare(a:Int64Native, b:Int64Native):Int {
		if ((a : jvm.Int64) < 0)
			return ((b : jvm.Int64) < 0) ? compare(a, b) : 1;
		return ((b : jvm.Int64) < 0) ? -1 : compare(a, b);
	}

	public static inline function neg(x:Int64Native):Int64Native
		return cast -(x : jvm.Int64);

	public static inline function add(a:Int64Native, b:Int64Native):Int64Native
		return cast((a : jvm.Int64) + (b : jvm.Int64));

	public static inline function sub(a:Int64Native, b:Int64Native):Int64Native
		return cast((a : jvm.Int64) - (b : jvm.Int64));

	public static inline function mul(a:Int64Native, b:Int64Native):Int64Native
		return cast((a : jvm.Int64) * (b : jvm.Int64));

	public static inline function divMod(dividend:Int64Native, divisor:Int64Native):{quotient:Int64Native, modulus:Int64Native}
		return {quotient: cast((dividend : jvm.Int64) / (divisor : jvm.Int64)), modulus: cast((dividend : jvm.Int64) % (divisor : jvm.Int64))};

	public static inline function eq(a:Int64Native, b:Int64Native):Bool
		return (a : jvm.Int64) == (b : jvm.Int64);

	public static inline function neq(a:Int64Native, b:Int64Native):Bool
		return (a : jvm.Int64) != (b : jvm.Int64);

	public static inline function complement(x:Int64Native):Int64Native
		return cast ~(x : jvm.Int64);

	public static inline function and(a:Int64Native, b:Int64Native):Int64Native
		return cast((a : jvm.Int64) & (b : jvm.Int64));

	public static inline function or(a:Int64Native, b:Int64Native):Int64Native
		return cast((a : jvm.Int64) | (b : jvm.Int64));

	public static inline function xor(a:Int64Native, b:Int64Native):Int64Native
		return cast((a : jvm.Int64) ^ (b : jvm.Int64));

	public static inline function shl(a:Int64Native, b:Int):Int64Native
		return cast((a : jvm.Int64) << b);

	public static inline function shr(a:Int64Native, b:Int):Int64Native
		return cast((a : jvm.Int64) >> b);

	public static inline function ushr(a:Int64Native, b:Int):Int64Native
		return cast((a : jvm.Int64) >>> b);

	public inline function toString():String
		return '${(this : jvm.Int64)}';

	public static inline function parseString(sParam:String):Int64Native {
		return haxe.numeric.Int64Helper.parseString(sParam);
	}

	public static inline function fromFloat(f:Float):Int64Native {
		return haxe.numeric.Int64Helper.fromFloat(f);
	}
}
