/*
 * Copyright (C)2005-2021 Haxe Foundation
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

import StringTools;

using haxe.UInt64;

/**
	Helper for parsing to `Int64` instances.
**/
class UInt64Helper {
	/**
		Create `Int64` from given string.
	**/
	public static function parseString(sParam:String):UInt64 {
		var base = UInt64.ofInt(10);
		var current = UInt64.ofInt(0);
		var multiplier = UInt64.ofInt(1);

		var s = StringTools.trim(sParam);
		if (s.charAt(0) == "-") {
			throw "NumberFormatError: Negative";
		}
		var len = s.length;

		for (i in 0...len) {
			var digitInt = s.charCodeAt(len - 1 - i) - '0'.code;
			if (digitInt < 0 || digitInt > 9) {
				throw "NumberFormatError";
			}
			if (digitInt != 0) {
				var digit:UInt64 = UInt64.ofInt(digitInt);
				current = UInt64.add(current, UInt64.mul(multiplier, digit));
			}
			multiplier = UInt64.mul(multiplier, base);
		}
		return current;
	}
}
