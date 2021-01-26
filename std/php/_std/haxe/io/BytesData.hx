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

using php.Syntax;

typedef BytesData = BytesDataAbstract;

private class Container {
	public var s:NativeString;

	public inline function new(s:NativeString)
		this.s = s;
}

private abstract BytesDataAbstract(Container) from Container to Container {
	public var length(get, never):Int;

	public static inline function alloc(length:Int):BytesDataAbstract {
		return Global.str_repeat(Global.chr(0), length);
	}

	@:arrayAccess
	public inline function get(pos:Int):Int {
		return Global.ord(this.s[pos]);
	}

	@:arrayAccess
	public inline function set(index:Int, val:Int):Void {
		this.s[index] = Global.chr(val);
	}

	public inline function compare(other:BytesDataAbstract):Int {
		return Syntax.spaceship(this.s, (other : Container).s);
	}

	public inline function getString(pos:Int, len:Int):String {
		return Global.substr(this.s, pos, len);
	}

	public inline function sub(pos:Int, len:Int):BytesDataAbstract {
		return (Global.substr(this.s, pos, len) : String);
	}

	public inline function blit(pos:Int, src:BytesDataAbstract, srcpos:Int, len:Int):Void {
		this.s = Global.substr(this.s, 0, pos).concat(Global.substr(src, srcpos, len)).concat(Global.substr(this.s, pos + len));
	}

	public inline function fill(pos:Int, len:Int, value:Int):Void {
		this.s = Global.substr(this.s, 0, pos).concat(Global.str_repeat(Global.chr(value), len)).concat(Global.substr(this.s, pos + len));
	}

	inline function get_length():Int {
		return Global.strlen(this.s);
	}

	@:from
	static inline function fromNativeString(s:NativeString):BytesDataAbstract {
		return new Container(s);
	}

	@:to
	public inline function toNativeString():NativeString {
		return this.s;
	}

	@:from
	static inline function fromString(s:String):BytesDataAbstract {
		return new Container(s);
	}

	@:to
	public inline function toString():String {
		return this.s;
	}
}
