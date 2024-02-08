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

import php.Global;
import php.Syntax;

class Bytes {
	public var length(default, null):Int;

	var b:BytesData;

	function new(length:Int, b:BytesData):Void {
		this.length = length;
		this.b = b;
	}

	public inline function get(pos:Int):Int {
		return b.get(pos);
	}

	public inline function set(pos:Int, v:Int):Void {
		b.set(pos, v);
	}

	public inline function blit(pos:Int, src:Bytes, srcpos:Int, len:Int):Void {
		if (pos < 0 || srcpos < 0 || len < 0 || pos + len > length || srcpos + len > src.length) {
			throw Error.OutsideBounds;
		} else {
			b.blit(pos, src.b, srcpos, len);
		}
	}

	public inline function fill(pos:Int, len:Int, value:Int):Void {
		b.fill(pos, len, value);
	}

	public inline function sub(pos:Int, len:Int):Bytes {
		if (pos < 0 || len < 0 || pos + len > length) {
			throw Error.OutsideBounds;
		} else {
			return new Bytes(len, b.sub(pos, len));
		}
	}

	public inline function compare(other:Bytes):Int {
		return b.compare(other.b);
	}

	public function getDouble(pos:Int):Float {
		return FPHelper.i64ToDouble(getInt32(pos), getInt32(pos + 4));
	}

	public function getFloat(pos:Int):Float {
		var b = new haxe.io.BytesInput(this, pos, 4);
		return b.readFloat();
	}

	public function setDouble(pos:Int, v:Float):Void {
		var i = FPHelper.doubleToI64(v);
		setInt32(pos, i.low);
		setInt32(pos + 4, i.high);
	}

	public function setFloat(pos:Int, v:Float):Void {
		setInt32(pos, FPHelper.floatToI32(v));
	}

	public inline function getUInt16(pos:Int):Int {
		return get(pos) | (get(pos + 1) << 8);
	}

	public inline function setUInt16(pos:Int, v:Int):Void {
		set(pos, v);
		set(pos + 1, v >> 8);
	}

	public inline function getInt32(pos:Int):Int {
		var v = get(pos) | (get(pos + 1) << 8) | (get(pos + 2) << 16) | (get(pos + 3) << 24);
		return if (v & 0x80000000 != 0) v | 0x80000000 else v;
	}

	public inline function getInt64(pos:Int):haxe.Int64 {
		return haxe.Int64.make(getInt32(pos + 4), getInt32(pos));
	}

	public inline function setInt32(pos:Int, v:Int):Void {
		set(pos, v);
		set(pos + 1, v >> 8);
		set(pos + 2, v >> 16);
		set(pos + 3, v >>> 24);
	}

	public inline function setInt64(pos:Int, v:haxe.Int64):Void {
		setInt32(pos, v.low);
		setInt32(pos + 4, v.high);
	}

	public inline function getString(pos:Int, len:Int, ?encoding:Encoding):String {
		if (pos < 0 || len < 0 || pos + len > length) {
			throw Error.OutsideBounds;
		} else {
			// no need to handle encoding, because PHP strings are binary safe.
			return b.getString(pos, len);
		}
	}

	@:deprecated("readString is deprecated, use getString instead")
	@:noCompletion
	public inline function readString(pos:Int, len:Int):String {
		return getString(pos, len);
	}

	public function toString():String {
		return b;
	}

	public inline function toHex():String {
		return php.Global.bin2hex(b.toString());
	}

	public inline function getData():BytesData {
		return b;
	}

	public static function alloc(length:Int):Bytes {
		return new Bytes(length, BytesData.alloc(length));
	}

	public static inline function ofString(s:String, ?encoding:Encoding):Bytes {
		return new Bytes(php.Global.strlen(s), s);
	}

	public static inline function ofData(b:BytesData):Bytes {
		return new Bytes(b.length, b);
	}

	public static function ofHex(s:String):Bytes {
		var len = Global.strlen(s);
		if ((len & 1) != 0)
			throw "Not a hex string (odd number of digits)";
		var b:String = php.Global.hex2bin(s);
		return new Bytes(Global.strlen(b), b);
	}

	public inline static function fastGet(b:BytesData, pos:Int):Int {
		return b.get(pos);
	}
}
