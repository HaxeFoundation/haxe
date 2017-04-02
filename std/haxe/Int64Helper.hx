/*
 * Copyright (C)2005-2017 Haxe Foundation
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
	public static function parseString( sParam : String ) : Int64 {
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
			multiplier = Int64.mul(multiplier, base);
		}
		return current;
	}

	/**
		Create `Int64` from given float.
	**/
	public static function fromFloat( f : Float ) : Int64 {
		if (Math.isNaN(f) || !Math.isFinite(f)) {
			throw "Number is NaN or Infinite";
		}

		var noFractions = f - (f % 1);

		// 2^53-1 and -2^53: these are parseable without loss of precision
		if (noFractions > 9007199254740991) {
			throw "Conversion overflow";
		}
		if (noFractions < -9007199254740991) {
			throw "Conversion underflow";
		}

		var result = Int64.ofInt(0);
		var neg = noFractions < 0;
		var rest = neg ? -noFractions : noFractions;

		var i = 0;
		while (rest >= 1) {
			var curr = rest % 2;
			rest = rest / 2;
			if (curr >= 1) {
				result = Int64.add(result, Int64.shl(Int64.ofInt(1), i));
			}
			i++;
		}

		if (neg) {
			result = Int64.neg(result);
		}
		return result;
	}
}
