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
	Shared unsigned-arithmetic helpers used by `Int32Native` and `Int32Direct`.

	These operate on raw `Int` values (the underlying representation of any
	`Int32Native` implementation) and therefore contain no target-specific logic.
**/
class Int32Helper {
	/**
		Convert a raw `Int` to `Float` treating its bit-pattern as an unsigned
		32-bit value. Values that appear negative as signed Int are converted as
		`4294967296 + v`.
	**/
	public static inline function utoFloat(v:Int):Float
		return v < 0 ? 4294967296.0 + v : v + 0.0;

	/**
		Perform unsigned integer division/modulo on raw `Int` values `a` and `b`.
		Returns the quotient and modulus as raw `Int`s (callers apply any needed
		masking). Throws on division by zero.
	**/
	public static inline function udivMod(a:Int, b:Int):{quotient:Int, modulus:Int} {
		var af = utoFloat(a);
		var bf = utoFloat(b);
		if (bf == 0)
			throw "Division by zero";
		return {quotient: Std.int(af / bf), modulus: Std.int(af % bf)};
	}

	/**
		Parse a signed decimal string into a raw `Int` in the range [-2^31, 2^31-1].
		Throws `NumberFormatError` on invalid input or out-of-range values.
	**/
	public static function parseString(s:String):Int {
		var t = StringTools.trim(s);
		if (t.length == 0)
			throw "NumberFormatError";
		var negative = t.charAt(0) == "-";
		var digits = negative ? t.substring(1) : t;
		if (digits.length == 0)
			throw "NumberFormatError";
		var result:Float = 0.0;
		for (i in 0...digits.length) {
			var d = digits.charCodeAt(i) - '0'.code;
			if (d < 0 || d > 9)
				throw "NumberFormatError";
			result = result * 10.0 + d;
		}
		if (negative)
			result = -result;
		if (result < -2147483648.0 || result > 2147483647.0)
			throw "NumberFormatError: Overflow";
		return Std.int(result);
	}

	/**
		Parse an unsigned decimal string into a raw `Int` whose bit-pattern
		represents a value in the range [0, 2^32-1].
		Values ≥ 2^31 are stored as negative `Int` (two's complement).
		Throws `NumberFormatError` on invalid input or out-of-range values.
	**/
	public static function uparseString(s:String):Int {
		var t = StringTools.trim(s);
		if (t.length == 0 || t.charAt(0) == "-")
			throw "NumberFormatError";
		var result:Float = 0.0;
		for (i in 0...t.length) {
			var d = t.charCodeAt(i) - '0'.code;
			if (d < 0 || d > 9)
				throw "NumberFormatError";
			result = result * 10.0 + d;
		}
		if (result > 4294967295.0)
			throw "NumberFormatError: Overflow";
		// Values ≥ 2^31 must be stored as negative Int (bit-pattern preservation).
		if (result >= 2147483648.0)
			return Std.int(result - 4294967296.0);
		return Std.int(result);
	}
}
