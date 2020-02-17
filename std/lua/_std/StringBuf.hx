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

import lua.Table;

class StringBuf {
	var b:Table<Int, String>;

	public var length(get, null):Int;

	public inline function new() {
		b = Table.create();
		this.length = 0;
	}

	inline function get_length():Int {
		return length;
	}

	public inline function add<T>(x:T):Void {
		var str = Std.string(x);
		Table.insert(b, str);
		length += str.length;
	}

	public inline function addChar(c:Int):Void {
		Table.insert(b, String.fromCharCode(c));
		length += 1;
	}

	public inline function addSub(s:String, pos:Int, ?len:Int):Void {
		var part = len == null ? s.substr(pos) : s.substr(pos, len);
		Table.insert(b, part);
		length += part.length;
	}

	public inline function toString():String {
		return Table.concat(b);
	}
}
