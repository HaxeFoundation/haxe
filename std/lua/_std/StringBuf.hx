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

import lua.Lua;
import lua.Table;

class StringBuf {
	private var b:Table<Int, String>;

	/**
		Count of "good" elements in the internal buffer table.

		If `this` StringBuf has been `clear`ed previously,
		this value might not be equal to the length (`#`) of that table.
	**/
	private var bufferLength:Int;

	public var length(get, null):Int;

	public inline function new() {
		b = Table.create();
		this.bufferLength = 0;
		this.length = 0;
	}

	inline function get_length():Int {
		return length;
	}

	public inline function add<T>(x:T):Void {
		final str = Std.string(x);
		final i = this.bufferLength += 1;
		Lua.rawset(this.b, i, str);
		this.length += str.length;
	}

	public inline function addChar(c:Int):Void {
		final i = this.bufferLength += 1;
		Lua.rawset(this.b, i, String.fromCharCode(c));
		this.length += 1;
	}

	public inline function addSub(s:String, pos:Int, ?len:Int):Void {
		this.add(s.substr(pos, len));
	}

	public inline function clear():Void {
		this.bufferLength = 0;
		this.length = 0;
	}

	public inline function toString():String {
		final len = this.bufferLength;
		if (len == 0) {
			return "";
		}
		return Table.concat(this.b, "", 1, len);
	}
}
