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

typedef Int32Native = Int32NativeImpl;

private abstract Int32NativeImpl(Int) from Int to Int {
	public static inline function neg(x:Int32Native):Int32Native
		return clamp(~(x : Int) + 1);

	public static inline function add(a:Int32Native, b:Int32Native):Int32Native
		return clamp((a : Int) + (b : Int));

	public static inline function sub(a:Int32Native, b:Int32Native):Int32Native
		return clamp((a : Int) - (b : Int));

	#if (js || php || python || lua)
	#if js
	// On JS we want to try using Math.imul, but we have to assign that function
	// to _mul only once, or else V8 will deoptimize it.
	// See https://github.com/HaxeFoundation/haxe/issues/5367 for benchmarks.
	public static inline function mul(a:Int32Native, b:Int32Native):Int32Native
		return _mul(a, b);

	static var _mul:Int32Native->Int32Native->Int32Native = untyped if (Math.imul != null)
			Math.imul
		else
			function(a:Int32Native, b:Int32Native):Int32Native return clamp((a : Int) * ((b : Int) & 0xFFFF) + clamp((a : Int) * ((b : Int) >>> 16) << 16));
	#else
	public static function mul(a:Int32Native, b:Int32Native):Int32Native
		return clamp((a : Int) * ((b : Int) & 0xFFFF) + clamp((a : Int) * ((b : Int) >>> 16) << 16));
	#end
	#else
	public static inline function mul(a:Int32Native, b:Int32Native):Int32Native
		return clamp((a : Int) * (b : Int));
	#end

	#if (lua || python || php)
	public static #if (python || php) inline #end function complement(a:Int32Native):Int32Native
		#if lua return lua.Boot.clampInt32(~(a : Int)); #else return clamp(~(a : Int)); #end
	#else
	public static inline function complement(a:Int32Native):Int32Native
		return cast ~(a : Int);
	#end

	public static inline function and(a:Int32Native, b:Int32Native):Int32Native
		return cast((a : Int) & (b : Int));

	#if (lua || python || php)
	public static #if (python || php) inline #end function or(a:Int32Native, b:Int32Native):Int32Native
		return clamp((a : Int) | (b : Int));
	#else
	public static inline function or(a:Int32Native, b:Int32Native):Int32Native
		return cast((a : Int) | (b : Int));
	#end

	#if (lua || python || php)
	public static #if (python || php) inline #end function xor(a:Int32Native, b:Int32Native):Int32Native
		return clamp((a : Int) ^ (b : Int));
	#else
	public static inline function xor(a:Int32Native, b:Int32Native):Int32Native
		return cast((a : Int) ^ (b : Int));
	#end

	#if (php || python || lua)
	public static inline function shl(a:Int32Native, b:Int):Int32Native
		return clamp((a : Int) << b);
	#else
	public static inline function shl(a:Int32Native, b:Int):Int32Native
		return cast((a : Int) << b);
	#end

	#if (lua || python || php)
	public static #if (python || php) inline #end function shr(a:Int32Native, b:Int):Int32Native
		return clamp((a : Int) >> b);
	#else
	public static inline function shr(a:Int32Native, b:Int):Int32Native
		return cast((a : Int) >> b);
	#end

	public static inline function ushr(a:Int32Native, b:Int):Int32Native
		return cast((a : Int) >>> b);

	public static inline function compare(a:Int32Native, b:Int32Native):Int {
		var av:Int = a;
		var bv:Int = b;
		return av < bv ? -1 : (av > bv ? 1 : 0);
	}

	public static function ucompare(a:Int32Native, b:Int32Native):Int {
		if ((a : Int) < 0)
			return (b : Int) < 0 ? (~(b : Int) - ~(a : Int)) : 1;
		return (b : Int) < 0 ? -1 : ((a : Int) - (b : Int));
	}

	public static inline function mod(a:Int32Native, b:Int32Native):Int32Native
		return cast((a : Int) % (b : Int));

	/**
		Convert `a` to Float treating its bit-pattern as an unsigned 32-bit value.
		Values that appear negative as signed Int are converted as `4294967296 + a`.
	**/
	public static inline function utoFloat(a:Int32Native):Float
		return Int32Helper.utoFloat(a);

	/**
		Perform unsigned integer division/modulo on `a` and `b`.
		The quotient and modulus are computed treating both values as unsigned 32-bit integers.
		Throws on division by zero.
	**/
	public static inline function udivMod(a:Int32Native, b:Int32Native):{quotient:Int32Native, modulus:Int32Native} {
		var r = Int32Helper.udivMod(a, b);
		return {quotient: clamp(r.quotient), modulus: clamp(r.modulus)};
	}

	/**
		Returns the unsigned decimal string representation of `a`.
	**/
	public static inline function utoString(a:Int32Native):String {
		var v:Int = a;
		if (v >= 0)
			return Std.string(v);
		// High bit set: unsigned value = 2^32 + v.
		// Using integer arithmetic avoids float-format strings (e.g. "1.0" on Lua).
		// Safe here because this file is only used on scripting targets where Int > 32 bits.
		return Std.string(4294967296 + v);
	}

	public inline function toFloat():Float
		return this;

	#if php
	static var extraBits:Int = php.Const.PHP_INT_SIZE * 8 - 32;
	#end

	#if !lua
	inline
	#end
	public static function clamp(x:Int):Int32Native {
		#if js
		return cast(x | 0);
		#elseif php
		return cast((x << extraBits) >> extraBits);
		#elseif python
		return cast(((python.Syntax.code("{0} % {1}", (x + python.Syntax.opPow(2, 31)), python.Syntax.opPow(2, 32)) : Int) - python.Syntax.opPow(2, 31)));
		#elseif lua
		return cast lua.Boot.clampInt32(x);
		#else
		return cast x;
		#end
	}
}
