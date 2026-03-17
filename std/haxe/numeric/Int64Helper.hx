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
	Helper for parsing to `Int64Native` instances.
**/
class Int64Helper {
	/**
		Create `Int64Native` from given string.
	**/
	public static function parseString(sParam:String):Int64Native {
		var base = Int64Native.ofInt(10);
		var current = Int64Native.ofInt(0);
		var multiplier = Int64Native.ofInt(1);
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
				var digit = Int64Native.ofInt(digitInt);
				if (sIsNegative) {
					current = Int64Native.sub(current, Int64Native.mul(multiplier, digit));
					if (!Int64Native.isNeg(current)) {
						throw "NumberFormatError: Underflow";
					}
				} else {
					current = Int64Native.add(current, Int64Native.mul(multiplier, digit));
					if (Int64Native.isNeg(current)) {
						throw "NumberFormatError: Overflow";
					}
				}
			}

			multiplier = Int64Native.mul(multiplier, base);
		}
		return current;
	}

	/**
		Create `Int64Native` from given float.
	**/
	public static function fromFloat(f:Float):Int64Native {
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

		var result = Int64Native.ofInt(0);
		var neg = noFractions < 0;
		var rest = neg ? -noFractions : noFractions;

		var i = 0;
		while (rest >= 1) {
			var curr = rest % 2;
			rest = rest / 2;
			if (curr >= 1) {
				result = Int64Native.add(result, Int64Native.shl(Int64Native.ofInt(1), i));
			}
			i++;
		}

		if (neg) {
			result = Int64Native.neg(result);
		}
		return result;
	}
}
