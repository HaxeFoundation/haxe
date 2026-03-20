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
	Helpers for unsigned 64-bit integer operations on top of `Int64Native`.

	These implement the operations that differ between signed and unsigned
	interpretation: division, modulo, toString, parseString, and fromFloat.
	All other 64-bit operations (add, sub, mul, bitwise, shifts) are
	bit-identical for signed and unsigned and can use `Int64Native` directly.
**/
class UInt64Helper {
	/**
		Performs unsigned 64-bit integer division.
		Returns `{ quotient, modulus }` treating both operands as unsigned.

		This is the same algorithm as the signed `Int64Native.divMod` but
		without sign handling, demonstrating that unsigned operations can
		be built on top of the shared `Int64Native` representation.
	**/
	public static function udivMod(dividend:Int64Native, divisor:Int64Native):{quotient:Int64Native, modulus:Int64Native} {
		if (divisor.high == 0) {
			switch (divisor.low.toInt()) {
				case 0:
					throw "divide by zero";
				case 1:
					return {quotient: Int64Native.make(dividend.high, dividend.low), modulus: Int64Native.ofInt(0)};
			}
		}

		var modulus = Int64Native.make(dividend.high, dividend.low);
		var quotient = Int64Native.ofInt(0);
		var mask = Int64Native.ofInt(1);

		while (!Int64Native.isNeg(divisor)) {
			var cmp = Int64Native.ucompare(divisor, modulus);
			divisor = Int64Native.shl(divisor, 1);
			mask = Int64Native.shl(mask, 1);
			if (cmp >= 0)
				break;
		}

		while (!Int64Native.isZero(mask)) {
			if (Int64Native.ucompare(modulus, divisor) >= 0) {
				quotient = Int64Native.or(quotient, mask);
				modulus = Int64Native.sub(modulus, divisor);
			}
			mask = Int64Native.ushr(mask, 1);
			divisor = Int64Native.ushr(divisor, 1);
		}

		return {
			quotient: quotient,
			modulus: modulus
		};
	}

	/**
		Returns the unsigned decimal string representation of the given 64-bit value.

		Uses the same 16-bit chunk algorithm as the signed toString but
		treats all bits as unsigned (no negative sign handling).
	**/
	public static function utoString(x:Int64Native):String {
		if (x.high == 0 && x.low == 0)
			return "0";
		var d3 = (x.high >>> 16).toInt() & 0xFFFF;
		var d2 = x.high.toInt() & 0xFFFF;
		var d1 = (x.low >>> 16).toInt() & 0xFFFF;
		var d0 = x.low.toInt() & 0xFFFF;
		var str = "";
		while (d3 != 0 || d2 != 0 || d1 != 0 || d0 != 0) {
			var r = d3 % 10;
			d3 = Std.int(d3 / 10);
			var v = r * 65536 + d2;
			d2 = Std.int(v / 10);
			r = v % 10;
			v = r * 65536 + d1;
			d1 = Std.int(v / 10);
			r = v % 10;
			v = r * 65536 + d0;
			d0 = Std.int(v / 10);
			str = (v % 10) + str;
		}
		return str;
	}

	/**
		Parses an unsigned decimal string into an `Int64Native` value.
		Throws on invalid input, negative values, or overflow.
	**/
	public static function parseString(sParam:String):Int64Native {
		var base = Int64Native.ofInt(10);
		var current = Int64Native.ofInt(0);
		var multiplier = Int64Native.ofInt(1);

		var s = StringTools.trim(sParam);
		if (s.charAt(0) == "-")
			throw "NumberFormatError: negative value for unsigned type";

		var len = s.length;
		for (i in 0...len) {
			var digitInt = s.charCodeAt(len - 1 - i) - '0'.code;

			if (digitInt < 0 || digitInt > 9)
				throw "NumberFormatError";

			if (digitInt != 0) {
				var digit = Int64Native.ofInt(digitInt);
				var prev = current;
				current = Int64Native.add(current, Int64Native.mul(multiplier, digit));
				if (Int64Native.ucompare(current, prev) < 0)
					throw "NumberFormatError: Overflow";
			}

			multiplier = Int64Native.mul(multiplier, base);
		}
		return current;
	}

	/**
		Converts a non-negative `Float` to an unsigned `Int64Native` value.
		The float must be in the range `[0, 2^53-1]` to avoid precision loss.
		Throws on negative, NaN, Infinite, or out-of-range values.
	**/
	public static function fromFloat(f:Float):Int64Native {
		if (Math.isNaN(f) || !Math.isFinite(f))
			throw "Number is NaN or Infinite";

		var noFractions = f - (f % 1);

		if (noFractions < 0)
			throw "Conversion: negative value for unsigned type";

		if (noFractions > 9007199254740991)
			throw "Conversion overflow";

		var result = Int64Native.ofInt(0);
		var rest = noFractions;

		var i = 0;
		while (rest >= 1) {
			var curr = rest % 2;
			rest = rest / 2;
			if (curr >= 1) {
				result = Int64Native.add(result, Int64Native.shl(Int64Native.ofInt(1), i));
			}
			i++;
		}

		return result;
	}

	/**
		Converts an unsigned 64-bit value to its `Float` representation.
		All 64 bits are treated as an unsigned magnitude.
		Values above `2^53` may lose precision.
	**/
	public static function toFloat(x:Int64Native):Float {
		var f:Float = x.low.toFloat();
		if (f < 0)
			f += 4294967296.0;
		var h:Float = x.high.toFloat();
		if (h < 0)
			h += 4294967296.0;
		return h * 4294967296.0 + f;
	}
}
