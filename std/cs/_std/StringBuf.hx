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
import cs.system.text.StringBuilder;

@:coreApi
class StringBuf {

	private var b : StringBuilder;

	public var length(get,never) : Int;

	public inline function new() : Void {
		b = new StringBuilder();
	}

	inline function get_length() : Int {
		return b.Length;
	}

	public inline function add<T>( x : T ) : Void {
		b.Append(Std.string(x));
	}

	public inline function addSub( s : String, pos : Int, ?len : Int ) : Void {
		b.Append(s, pos, (len == null) ? (s.length - pos) : len);
	}

	public inline function addChar( c : Int ) : Void untyped {
		b.Append(cast(c, cs.StdTypes.Char16));
	}

	public inline function toString() : String {
		return b.ToString();
	}

}