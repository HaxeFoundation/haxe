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

import haxe.Int64;

/**
	Helper that converts between floating point and binary representation.
	Always works in low-endian encoding.
**/
class FPHelper {
	public static inline function i32ToFloat(i:Int):Float {
		return cs.system.BitConverter.Int32BitsToSingle(i);
	}

	public static inline function floatToI32(f:Float):Int {
		return cs.system.BitConverter.SingleToInt32Bits(f);
	}

	public static inline function i64ToDouble(low:Int, high:Int):Float {
		return cs.system.BitConverter.Int64BitsToDouble(Int64.make(high, low));
	}

	public static inline function doubleToI64(v:Float):Int64 {
		return cs.system.BitConverter.DoubleToInt64Bits(v);
	}
}
