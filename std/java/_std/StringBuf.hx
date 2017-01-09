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

@:coreApi
class StringBuf {

	private var b : java.lang.StringBuilder;

	public var length(get,never) : Int;

	public function new() : Void {
		b = new java.lang.StringBuilder();
	}

	inline function get_length() : Int {
		return b.length();
	}

	public function add<T>( x : T ) : Void {
		if (Std.is(x, Int))
		{
			var x:Int = cast x;
			var xd:Dynamic = x;
			b.append(xd);
		} else {
			b.append(x);
		}
	}

	public function addSub( s : String, pos : Int, ?len : Int ) : Void {
		var l:Int = (len == null) ? s.length - pos : len;
		b.append(s, pos, pos + l);
	}

	public function addChar( c : Int ) : Void untyped {
		b.append(cast(c, java.StdTypes.Char16));
	}

	public function toString() : String {
		return b.toString();
	}

}
