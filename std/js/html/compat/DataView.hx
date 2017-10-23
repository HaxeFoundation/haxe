/*
 * Copyright (C)2005-2018 Haxe Foundation
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
package js.html.compat;

#if !nodejs
import haxe.io.Error;

@:ifFeature("js.html.DataView.*")
@:access(js.html.compat.ArrayBuffer)
class DataView {

	var buf : ArrayBuffer;
	var offset : Int;
	var length : Int;

	public var byteLength(default,null):Int;
	public var byteOffset(default,null):Int;
	public var buffer(default,null):ArrayBuffer;

	public function new( buffer : ArrayBuffer, ?byteOffset : Int, ?byteLength : Int ) : Void {
		this.buf = buffer;
		this.offset = byteOffset == null ? 0 : byteOffset;
		this.length = byteLength == null ? buffer.byteLength - this.offset : byteLength;
		if( offset < 0 || length < 0 || offset+length > buffer.byteLength )
			throw OutsideBounds;
		this.byteLength = length;
		this.byteOffset = offset;
		this.buffer = buf;
	}

	public function getInt8( byteOffset : Int ) : Int {
		var v = buf.a[offset + byteOffset];
		return v >= 0x80 ? v - 256 : v;
	}

	public function getUint8( byteOffset : Int ) : Int {
		return buf.a[offset + byteOffset];
	}

	public function getInt16( byteOffset : Int, ?littleEndian : Bool ) : Int {
		var v = getUint16(byteOffset, littleEndian);
		return v >= 0x8000 ? v - 65536 : v;
	}

	public function getUint16( byteOffset : Int, ?littleEndian : Bool ) : Int {
		return littleEndian ? buf.a[offset + byteOffset] | (buf.a[offset + byteOffset + 1] << 8) : (buf.a[offset + byteOffset]<<8) | buf.a[offset + byteOffset + 1];
	}

	public function getInt32( byteOffset : Int, ?littleEndian : Bool ) : Int {
		var p = offset + byteOffset;
		var a = buf.a[p++];
		var b = buf.a[p++];
		var c = buf.a[p++];
		var d = buf.a[p++];
		return littleEndian ? a | (b<<8) | (c<<16) | (d<<24) : d | (c << 8) | (b << 16) | (a << 24);
	}

	public function getUint32( byteOffset : Int, ?littleEndian : Bool ) : Int {
		var v = getInt32(byteOffset, littleEndian);
		return v < 0 ? cast (v + 4294967296.) : v;
	}

	public function getFloat32( byteOffset : Int, ?littleEndian : Bool ) : Float {
		var i = getInt32(byteOffset, littleEndian);
		var sign = 0x80000000 & i == 0 ? 1.0 : -1.0;
		var e = (i >> 23) & 0xff;
		if (e == 255)
			return i & 0x7fffff == 0
				? (sign > 0 ? Math.POSITIVE_INFINITY : Math.NEGATIVE_INFINITY)
				: Math.NaN;
		var m = e == 0 ? (i & 0x7fffff) << 1 : (i & 0x7fffff) | 0x800000;
		return sign * m * Math.pow(2, e - 150);
	}

	public function getFloat64( byteOffset : Int, ?littleEndian : Bool ) : Float {
		var lo = 0;
		var hi = 0;
		if (littleEndian) {
			lo = getInt32(byteOffset    , true);
			hi = getInt32(byteOffset + 4, true);
		} else {
			hi = getInt32(byteOffset    , false);
			lo = getInt32(byteOffset + 4, false);
		}
		var sign = 0x80000000 & hi == 0 ? 1.0 : -1.0;
		var e = (hi >> 20) & 0x7ff;
		if (e == 2047)
			return lo == 0 && (hi & 0xFFFFF) == 0
				? (sign > 0 ? Math.POSITIVE_INFINITY : Math.NEGATIVE_INFINITY)
				: Math.NaN;
		var m = Math.pow(2, -52) * ((hi & 0xFFFFF) * 4294967296. + (lo >>> 31) * 2147483648. + (lo & 0x7FFFFFFF));
		m = e == 0 ? m * 2.0 : m + 1.0;
		return sign * m * Math.pow(2, e - 1023);
	}

	public function setInt8( byteOffset : Int, value : Int ) : Void {
		buf.a[byteOffset + offset] = (value < 0) ? (value + 128) & 0xFF : value & 0xFF;
	}

	public function setUint8( byteOffset : Int, value : Int ) : Void {
		buf.a[byteOffset + offset] = value & 0xFF;
	}

	public function setInt16( byteOffset : Int, value : Int, ?littleEndian : Bool ) : Void {
		setUint16(byteOffset, value < 0 ? value + 65536 : value, littleEndian);
	}

	public function setUint16( byteOffset : Int, value : Int, ?littleEndian : Bool ) : Void {
		var p = byteOffset + offset;
		if( littleEndian ) {
			buf.a[p++] = value&0xFF;
			buf.a[p] = (value>>8) & 0xFF;
		} else {
			buf.a[p++] = (value>>8) & 0xFF;
			buf.a[p] = value&0xFF;
		}
	}

	public function setInt32( byteOffset : Int, value : Int, ?littleEndian : Bool ) : Void {
		setUint32(byteOffset, value, littleEndian);
	}

	public function setUint32( byteOffset : Int, value : Int, ?littleEndian : Bool ) : Void {
		var p = byteOffset + offset;
		if( littleEndian ) {
			buf.a[p++] = value & 0xFF;
			buf.a[p++] = (value>>8) & 0xFF;
			buf.a[p++] = (value>>16) & 0xFF;
			buf.a[p++] = value >>> 24;
		} else {
			buf.a[p++] = value >>> 24;
			buf.a[p++] = (value>>16) & 0xFF;
			buf.a[p++] = (value>>8) & 0xFF;
			buf.a[p++] = value & 0xFF;
		}
	}

	static inline var LN2 = 0.6931471805599453;
	public function setFloat32( byteOffset : Int, f : Float, ?littleEndian : Bool ) : Void {
		if( f == 0 ) setUint32(byteOffset, 0, littleEndian);
		var af = f < 0 ? -f : f;
		var exp = Math.floor(Math.log(af) / LN2);
		if ( exp <= -127 ) {
			exp = -127;
			af = af / Math.pow(2, exp) * 0.5;
		}else if ( exp > 127 ) {
			setUint32(byteOffset, 0x7F800000, littleEndian);
		} else {
			af = af / Math.pow(2, exp) - 1.0;
		}
		var sig = Math.round(af * 0x800000);
		setUint32(byteOffset, (f < 0 ? 0x80000000 : 0) | ((exp + 127) << 23) | sig , littleEndian);
	}

	public function setFloat64( byteOffset : Int, v : Float, ?littleEndian : Bool ) : Void {
		var lo = 0;
		var hi = 0;
		if (!Math.isFinite(v)) {
			lo = 0;
			hi = v > 0 ? 0x7FF00000 : 0xFFF00000;
		} else if (v != 0) {
			var av = v < 0 ? -v : v;
			var exp = Math.floor(Math.log(av) / LN2);
			if (exp > 1023) {  // MAX_VALUE
				lo = 0xFFFFFFFF;
				hi = 0x7FEFFFFF;
			} else {
				 if (exp <= -1023) {
					exp = -1023;
					av = av / Math.pow(2, exp) * 0.5;
				 } else {
					av = av / Math.pow(2, exp) - 1.0;
				 }
				var sig = Math.fround(av * 4503599627370496.); // 2^52
				// Note: If "sig" is outside of the signed Int32 range, or is NaN, NEGATIVE_INFINITY or POSITIVE_INFINITY, the result is unspecified.
				// Works in JS, Lua, Flash, Python and Cpp
				lo = Std.int(sig);
				hi = (v < 0 ? 0x80000000 : 0) | ((exp + 1023) << 20) | Std.int(sig / 4294967296.0);
			}
		}
		if( littleEndian ) {
			setUint32(byteOffset,     lo, true);
			setUint32(byteOffset + 4, hi, true);
		} else {
			setUint32(byteOffset,     hi, false);
			setUint32(byteOffset + 4, lo, false);
		}
	}

	static function __init__() {
		untyped __js__("var DataView = {0} || {1}", js.Lib.global.DataView, js.html.compat.DataView);
	}

}
#end