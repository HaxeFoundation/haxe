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

import php.*;
import haxe.io.Error;

class BytesBuffer {
	var b:NativeString;

	public var length(get, never):Int;

	public function new() {
		b = "";
	}

	public inline function addByte(byte:Int) {
		b = Syntax.concat(b, Global.chr(byte));
	}

	public inline function add(src:Bytes) {
		b = Syntax.concat(b, src.getData().toNativeString());
	}

	public inline function addString(v:String, ?encoding:Encoding) {
		b = Syntax.concat(b, v);
	}

	public function addInt32(v:Int) {
		addByte(v & 0xFF);
		addByte((v >> 8) & 0xFF);
		addByte((v >> 16) & 0xFF);
		addByte(v >>> 24);
	}

	public function addInt64(v:haxe.Int64) {
		addInt32(v.low);
		addInt32(v.high);
	}

	public inline function addFloat(v:Float) {
		addInt32(FPHelper.floatToI32(v));
	}

	public inline function addDouble(v:Float) {
		addInt64(FPHelper.doubleToI64(v));
	}

	public inline function addBytes(src:Bytes, pos:Int, len:Int) {
		if (pos < 0 || len < 0 || pos + len > src.length) {
			throw Error.OutsideBounds;
		} else {
			b = Syntax.concat(b, src.getData().sub(pos, len).toString());
		}
	}

	public function getBytes():Bytes {
		var bytes = @:privateAccess new Bytes(length, b);
		b = null;
		return bytes;
	}

	inline function get_length():Int {
		return Global.strlen(b);
	}
}
