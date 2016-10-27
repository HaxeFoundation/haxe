/*
 * Copyright (C)2005-2016 Haxe Foundation
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

import php7.*;

abstract BytesData(NativeString) from NativeString to NativeString from String to String {

	public var length (get,never):Int;

	public static inline function alloc (length:Int):BytesData {
		return Global.str_repeat(Global.chr(0), length);
	}

	@:arrayAccess
	public inline function get (pos:Int):Int {
		return Global.ord(this[pos]);
	}

	@:arrayAccess
	public inline function set (index:Int, val:Int):Void {
		this[index] = Global.chr(val);
	}

	public inline function compare (other:BytesData):Int {
		return Syntax.binop(this, '<=>', other);
	}

	public inline function getString (pos:Int, len:Int):String {
		return Global.substr(this, pos, len);
	}

	public inline function sub (pos:Int, len:Int):BytesData {
		return (Global.substr(this, pos, len):String);
	}

	public inline function blit (pos : Int, src : BytesData, srcpos : Int, len : Int):Void {
		this = Syntax.binop(Syntax.binop(Global.substr(this, 0, pos), '.', Global.substr(src, srcpos, len)), '.', Global.substr(this, pos + len));
	}

	inline function get_length ():Int {
		return Global.strlen(this);
	}
}