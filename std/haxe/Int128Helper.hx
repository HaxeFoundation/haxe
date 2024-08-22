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

using haxe.Int128;

/**
	Helper for parsing to `Int128` instances.
**/
class Int128Helper {
    /**
        The maximum positive `Int128` value.
     */
    public static var maxValue:Int128 = Int128.make(Int64Helper.maxValue, Int64Helper.maxValue);

	/**
		Create `Int128` from given string.
	**/
	public static function parseString(sParam:String):Int128 {
		var base = Int128.ofInt64(10);
		var current = Int128.ofInt64(0);
		var multiplier = Int128.ofInt64(1);
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
				var digit:Int128 = Int128.ofInt64(digitInt);
				if (sIsNegative) {
					current -= multiplier * digit;
					if (!Int128.isNeg(current)) {
						throw "NumberFormatError: Underflow";
					}
				} else {
					current += multiplier * digit;
					if (Int128.isNeg(current)) {
						throw "NumberFormatError: Overflow";
					}
				}
			}

			multiplier *= base;
		}
		return current;
	}

	/**
		Create `Int128` from given float.
	**/
	public static function fromFloat(f:Float):Int128 {
		if (Math.isNaN(f) || !Math.isFinite(f)) {
			throw "Number is NaN or Infinite";
		}

		// Here's a funny story for someguywholovescoding
    // I thought Int64's noFractions calculation (which is float flooring with modulous) was faster than Math.ffloor but then I tested it.
		var noFractions = Math.ffloor(f);

		// 2^53-1 and -2^53+1: these are parseable without loss of precision.
		// In theory 2^53 and -2^53 are parseable too, but then there's no way to
		// distinguish 2^53 from 2^53+1
		// (i.e. trace(9007199254740992. + 1. > 9007199254740992.); // false!)
		if (noFractions > 9007199254740991) {
			throw "Conversion overflow";
		}
		if (noFractions < -9007199254740991) {
			throw "Conversion underflow";
		}

		var result = Int128.ofInt(0);
		var neg = noFractions < 0;
		var rest = neg ? -noFractions : noFractions;

		var i = 0;
		while (rest >= 1) {
			var curr = rest % 2;
			rest = rest / 2;
			if (curr >= 1) {
				result += Int128.ofInt(1) << i;
			}
			i++;
		}

		if (neg) {
			result = -result;
		}
		return result;
	}
}
