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
package haxe.io;

import php.*;

/**
	Helper that converts between floating point and binary representation.
	Always works in low-endian encoding.
**/
class FPHelper {

	static var isLittleEndian : Bool = Global.unpack('S','\x01\x00')[1] == 1;
	static var i64tmp = Int64.ofInt(0);

	public static inline function i32ToFloat( i : Int ) : Float {
		return Global.unpack('f', Global.pack('l', i))[1];
	}

	public static inline function floatToI32( f : Float ) : Int {
		return Global.unpack('l', Global.pack('f', f))[1];
	}

	public static inline function i64ToDouble( low : Int, high : Int ) : Float {
		return Global.unpack('d', Global.pack('ii', isLittleEndian ? low : high, isLittleEndian ? high : low))[1];
	}

	/**
		Returns an Int64 representing the bytes representation of the double precision IEEE float value.
		WARNING : for performance reason, the same Int64 value might be reused every time. Copy its low/high values before calling again.
		We still ensure that this is safe to use in a multithread environment
	**/
	public static function doubleToI64( v : Float ) : Int64 {
		var a = Global.unpack(isLittleEndian ? 'V2' : 'N2', Global.pack('d', v));
		var i64 = i64tmp;
		@:privateAccess i64.set_low(a[isLittleEndian ? 1 : 2]);
		@:privateAccess i64.set_high(a[isLittleEndian ? 2 : 1]);

		return i64;
	}

}