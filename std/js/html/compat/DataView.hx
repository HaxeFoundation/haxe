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
		return haxe.io.FPHelper.i32ToFloat(getInt32(byteOffset,littleEndian));
	}

	public function getFloat64( byteOffset : Int, ?littleEndian : Bool ) : Float {
		var a = getInt32(byteOffset, littleEndian);
		var b = getInt32(byteOffset + 4, littleEndian);
		return haxe.io.FPHelper.i64ToDouble(littleEndian?a:b,littleEndian?b:a);
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
			buf.a[p] = value&0xFF;
			buf.a[p++] = (value>>8) & 0xFF;
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

	public function setFloat32( byteOffset : Int, value : Float, ?littleEndian : Bool ) : Void {
		setUint32(byteOffset, haxe.io.FPHelper.floatToI32(value),littleEndian);
	}

	public function setFloat64( byteOffset : Int, value : Float, ?littleEndian : Bool ) : Void {
		var i64 = haxe.io.FPHelper.doubleToI64(value);
		if( littleEndian ) {
			setUint32(byteOffset, i64.low);
			setUint32(byteOffset, i64.high);
		} else {
			setUint32(byteOffset, i64.high);
			setUint32(byteOffset, i64.low);
		}
	}

	static function __init__() {
		var DataView = untyped js.Lib.global.DataView || js.html.compat.DataView;
	}

}
#end