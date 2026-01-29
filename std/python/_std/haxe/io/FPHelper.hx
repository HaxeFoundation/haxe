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

package haxe.io;

import python.lib.Struct;

/**
	Helper that converts between floating point and binary representation.
	Always works in low-endian encoding.
**/
class FPHelper {
	static var i64tmp:Int64 = Int64.ofInt(0);

	public static inline function i32ToFloat(i:Int):Float {
		// Pack as little-endian 32-bit signed int, unpack as float
		return Struct.unpack("<f", Struct.pack("<i", i))[0];
	}

	public static inline function floatToI32(f:Float):Int {
		// Pack as float, unpack as little-endian 32-bit signed int
		return Struct.unpack("<i", Struct.pack("<f", f))[0];
	}

	public static inline function i64ToDouble(low:Int, high:Int):Float {
		// Pack two 32-bit ints as little-endian, unpack as double
		return Struct.unpack("<d", Struct.pack("<ii", low, high))[0];
	}

	/**
		Returns an Int64 representing the bytes representation of the double precision IEEE float value.
		WARNING : for performance reason, the same Int64 value might be reused every time. Copy its low/high values before calling again.
		We still ensure that this is safe to use in a multithread environment.
	**/
	public static function doubleToI64(v:Float):Int64 @:privateAccess {
		// Pack as double, unpack as two little-endian 32-bit signed ints
		var ints = Struct.unpack("<ii", Struct.pack("<d", v));
		i64tmp.set_low(ints[0]);
		i64tmp.set_high(ints[1]);
		return i64tmp;
	}
}
