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

import eval.integers.Int64 as EvalInt64;

/**
	Eval/interpreter-native 64-bit integer implementation.
	Shadows the cross-platform emulation with the eval runtime's native
	`eval.integers.Int64` (`VInt64` OCaml value type).
**/
@:coreApi(check = Off)
abstract Int64Native(EvalInt64) from EvalInt64 to EvalInt64 {
	public var high(get, set):haxe.Int32;

	inline function get_high():haxe.Int32
		return (this >> 32).toInt32();

	inline function set_high(v:haxe.Int32):haxe.Int32 {
		this = EvalInt64.make(v, get_low());
		return v;
	}

	public var low(get, set):haxe.Int32;

	inline function get_low():haxe.Int32
		return this.toInt32();

	inline function set_low(v:haxe.Int32):haxe.Int32 {
		this = EvalInt64.make(get_high(), v);
		return v;
	}

	public inline function new(high:haxe.Int32, low:haxe.Int32) {
		this = EvalInt64.make(high, low);
	}

	public static inline function make(high:haxe.Int32, low:haxe.Int32):Int64Native {
		return new Int64Native(high, low);
	}

	public static inline function ofInt(x:Int):Int64Native {
		return cast EvalInt64.ofInt(x);
	}

	public static inline function toInt(x:Int64Native):Int {
		if (x.high != x.low >> 31)
			throw "Overflow";
		return (x : EvalInt64).toInt();
	}

	public static inline function isInt64(val:Dynamic):Bool {
		return Std.isOfType(val, EvalInt64);
	}

	public static inline function isNeg(x:Int64Native):Bool
		return (x : EvalInt64) < EvalInt64.ZERO;

	public static inline function isZero(x:Int64Native):Bool
		return (x : EvalInt64) == EvalInt64.ZERO;

	public static inline function compare(a:Int64Native, b:Int64Native):Int
		return EvalInt64.compare(a, b);

	public static function ucompare(a:Int64Native, b:Int64Native):Int {
		if ((a : EvalInt64) < EvalInt64.ZERO)
			return ((b : EvalInt64) < EvalInt64.ZERO) ? EvalInt64.compare(a, b) : 1;
		return ((b : EvalInt64) < EvalInt64.ZERO) ? -1 : EvalInt64.compare(a, b);
	}

	public static inline function neg(x:Int64Native):Int64Native
		return cast -(x : EvalInt64);

	public static inline function add(a:Int64Native, b:Int64Native):Int64Native
		return cast((a : EvalInt64) + (b : EvalInt64));

	public static inline function sub(a:Int64Native, b:Int64Native):Int64Native
		return cast((a : EvalInt64) - (b : EvalInt64));

	public static inline function mul(a:Int64Native, b:Int64Native):Int64Native
		return cast((a : EvalInt64) * (b : EvalInt64));

	public static inline function divMod(dividend:Int64Native, divisor:Int64Native):{quotient:Int64Native, modulus:Int64Native}
		return {quotient: cast((dividend : EvalInt64) / (divisor : EvalInt64)), modulus: cast((dividend : EvalInt64) % (divisor : EvalInt64))};

	public static inline function eq(a:Int64Native, b:Int64Native):Bool
		return (a : EvalInt64) == (b : EvalInt64);

	public static inline function neq(a:Int64Native, b:Int64Native):Bool
		return (a : EvalInt64) != (b : EvalInt64);

	public static inline function complement(x:Int64Native):Int64Native
		return cast ~(x : EvalInt64);

	public static inline function and(a:Int64Native, b:Int64Native):Int64Native
		return cast((a : EvalInt64) & (b : EvalInt64));

	public static inline function or(a:Int64Native, b:Int64Native):Int64Native
		return cast((a : EvalInt64) | (b : EvalInt64));

	public static inline function xor(a:Int64Native, b:Int64Native):Int64Native
		return cast((a : EvalInt64) ^ (b : EvalInt64));

	public static inline function shl(a:Int64Native, b:Int):Int64Native
		return cast((a : EvalInt64) << b);

	public static inline function shr(a:Int64Native, b:Int):Int64Native
		return cast((a : EvalInt64) >> b);

	public static inline function ushr(a:Int64Native, b:Int):Int64Native
		return cast((a : EvalInt64) >>> b);

	public inline function toString():String
		return this.toString();

	public static inline function parseString(sParam:String):Int64Native {
		return haxe.numeric.Int64Helper.parseString(sParam);
	}

	public static inline function fromFloat(f:Float):Int64Native {
		return haxe.numeric.Int64Helper.fromFloat(f);
	}
}
