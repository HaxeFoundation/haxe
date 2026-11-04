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

using haxe.Int64;

import StringTools;

/**
	Helper for parsing to `Int64` instances.
**/
class Int64Helper {
	/**
		Create `Int64` from given string.
	**/
	public static function parseString(sParam:String):Int64 {
		var base = Int64.ofInt(10);
		var current = Int64.ofInt(0);
		var multiplier = Int64.ofInt(1);
		var sIsNegative = false;

		var s = StringTools.trim(sParam);
		if (s.charAt(0) == "-") {
			sIsNegative = true;
			s = s.substring(1, s.length);
		}
		var len = s.length;

		for (i in 0...len) {
			var digitInt = s.charCodeAt(len - 1 - i) - '0'.code;

			if (digitInt < 0 || digitInt > 9) {
				throw "NumberFormatError";
			}

			if (digitInt != 0) {
				var digit:Int64 = Int64.ofInt(digitInt);
				if (sIsNegative) {
					current = Int64.sub(current, Int64.mul(multiplier, digit));
					if (!Int64.isNeg(current)) {
						throw "NumberFormatError: Underflow";
					}
				} else {
					current = Int64.add(current, Int64.mul(multiplier, digit));
					if (Int64.isNeg(current)) {
						throw "NumberFormatError: Overflow";
					}
				}
			}

			multiplier = Int64.mul(multiplier, base);
		}
		return current;
	}

	/**
		Create `Int64` from given float.
	**/
	public static function fromFloat(f:Float):Int64 {
		if (Math.isNaN(f) || !Math.isFinite(f)) {
			throw "Number is NaN or Infinite";
		}

		var noFractions = f - (f % 1);

		// 2^53-1 and -2^53+1: these are parsable without loss of precision.
		// In theory 2^53 and -2^53 are parsable too, but then there's no way to
		// distinguish 2^53 from 2^53+1
		// (i.e. trace(9007199254740992. + 1. > 9007199254740992.); // false!)
		if (noFractions > 9007199254740991) {
			throw "Conversion overflow";
		}
		if (noFractions < -9007199254740991) {
			throw "Conversion underflow";
		}

		#if python
		return ofNativeInt(python.Syntax.code("int({0})", noFractions));
		#elseif php
		if (php.Const.PHP_INT_SIZE == 8)
			return ofNativeInt(php.Syntax.int(noFractions));
		#elseif (lua && lua_ver >= 5.3)
		if (lua.Math.maxinteger > 2147483647)
			return ofNativeInt(lua.Math.tointeger(noFractions));
		#end
		var neg = noFractions < 0;
		var rest = neg ? -noFractions : noFractions;
		// rest < 2^53: split it exactly in two 32-bit words
		var high = Math.ffloor(rest / 4294967296.);
		var low = rest - high * 4294967296.;
		if (low >= 2147483648.)
			low -= 4294967296.;
		var result = Int64.make(Std.int(high), Std.int(low));
		return neg ? Int64.neg(result) : result;
	}

	/**
		Converts an `Int64` to a `Float`.

		Loss of precision may occur for values whose magnitude exceeds 2^53.
	**/
	public static function toFloat(x:Int64):Float {
		var high = x.high;
		var low = x.low;
		// `low` is treated as an unsigned 32-bit word; both terms are exactly
		// representable as Float, so the single addition yields the correctly
		// rounded result.
		return high * 4294967296. + (low < 0 ? 4294967296. + low : low);
	}

	#if (php || python || lua)
	/**
		Creates an `Int64` from `v`, a native integer (possibly wider than 32 bits) of this target.
	**/
	@:noCompletion
	static function ofNativeInt(v:Int):Int64 {
		#if php
		// on 32-bit PHP, values beyond the int range are floats
		if (php.Const.PHP_INT_SIZE < 8)
			return php.Global.is_float(v) ? fromFloat(cast v) : Int64.ofInt(v);
		return Int64.make(php.Syntax.code("{0} >> 32", v), php.Syntax.code("(({0} & 0xFFFFFFFF) ^ 0x80000000) - 0x80000000", v));
		#elseif python
		// ints are unbounded: wrap both words to 32 bits
		return Int64.make(python.Syntax.code("((({0} >> 32) & 0xFFFFFFFF) ^ 0x80000000) - 0x80000000", v),
			python.Syntax.code("(({0} & 0xFFFFFFFF) ^ 0x80000000) - 0x80000000", v));
		#elseif (lua_ver >= 5.3)
		// Lua can be built with 32-bit integers (LUA_32BITS)
		if (lua.Math.maxinteger <= 2147483647)
			return fromFloat(v);
		// normalize a float subtype so both words are integers
		var v = lua.Math.tointeger(v);
		if (v == null)
			throw "Conversion overflow";
		return Int64.make(lua.Syntax.code("{0} // 4294967296", v), lua.Syntax.code("(({0} & 0xFFFFFFFF) ~ 0x80000000) - 0x80000000", v));
		#else
		// numbers are doubles
		return fromFloat(v);
		#end
	}

	/**
		Converts `x` to a native integer of this target. The result may exceed the 32-bit `Int` range.
	**/
	@:noCompletion
	static function toNativeInt(x:Int64):Int {
		#if php
		if (php.Const.PHP_INT_SIZE < 8)
			return cast toFloat(x);
		return php.Syntax.code("({0} << 32) | ({1} & 0xFFFFFFFF)", x.high, x.low);
		#elseif python
		return python.Syntax.code("({0} << 32) | ({1} & 0xFFFFFFFF)", x.high, x.low);
		#elseif (lua_ver >= 5.3)
		if (lua.Math.maxinteger <= 2147483647)
			return cast toFloat(x);
		return lua.Syntax.code("({0} << 32) | ({1} & 0xFFFFFFFF)", x.high, x.low);
		#else
		return cast toFloat(x);
		#end
	}
	#end
}
