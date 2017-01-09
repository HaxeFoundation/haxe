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

import python.lib.io.IOBase.SeekSet;
import python.lib.io.StringIO;

@:coreApi
class StringBuf {

	private var b : StringIO;

	public inline function new():Void {
		this.b = new StringIO();
	}

	public var length(get, never):Int;

	function get_length ():Int {
		var pos = b.tell();
		b.seek(0, SeekEnd);
		var len = b.tell();
		b.seek(pos, SeekSet);
		return len;
	}

	public inline function add<T>( x : T ) : Void {
		add1(Std.string(x));
	}

	inline function add1(s:String):Void {
		b.write(s);
	}

	public inline function addChar( c : Int ) : Void {
		add1(String.fromCharCode(c));
	}

	public inline function addSub( s : String, pos : Int, ?len : Int) : Void {
		add1((len == null ? s.substr(pos) : s.substr(pos, len)));
	}

	public inline function toString() : String {
		return b.getvalue();
	}
}
