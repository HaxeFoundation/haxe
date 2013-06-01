/*
 * Copyright (C)2005-2012 Haxe Foundation
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

	private var b : Array<String>;

	public var length(get,never) : Int;

	public function new() : Void {
		b = new Array();
	}

	function get_length() : Int {
		var len = 0;
		for(s in b) len += s.length;
		return len;
	}

	public function add( x : Dynamic ) : Void {
		b.push(x);
	}

	public inline function addSub( s : String, pos : Int, ?len : Int ) : Void {
		b.push(s.substr(pos,len));
	}

	public inline function addChar( c : Int ) : Void untyped {
		b.push(String.fromCharCode(c));
	}

	public inline function toString() : String {
		return b.join("");
	}

}
