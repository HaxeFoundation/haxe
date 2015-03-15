/*
 * Copyright (C)2005-2015 Haxe Foundation
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


/**
	A String buffer is an efficient way to build a big string by appending small
	elements together.

	Its cross-platform implementation uses String concatenation internally, but
	StringBuf may be optimized for different targets.

	Unlike String, an instance of StringBuf is not immutable in the sense that
	it can be passed as argument to functions which modify it by appending more
	values. However, the internal buffer cannot be modified.
**/

@:coreApi
class StringBuf {

	/**
		Creates a new StringBuf instance.

		This may involve initialization of the internal buffer.
	**/

	private var b : StringIO;

	public inline function new():Void {
		this.b = new StringIO();
	}

	public var length(get, never):Int;

	public function get_length ():Int {
		var pos = b.tell();
		b.seek(0, SeekEnd);
		var len = b.tell();
		b.seek(pos, SeekSet);
		return len;
	}

	/**
		Appends the representation of [x] to [this] StringBuf.

		The exact representation of [x] may vary per platform. To get more
		consistent behavior, this function should be called with
		Std.string(x).

		If [x] is null, the String "null" is appended.
	**/
	public inline function add<T>( x : T ) : Void {
		add1(Std.string(x));
	}

	inline function add1(s:String):Void {
		b.write(s);
	}

	/**
		Appends the character identified by [c] to [this] StringBuf.

		If [c] is negative or has another invalid value, the result is
		unspecified.
	**/
	public inline function addChar( c : Int ) : Void {
		add1(String.fromCharCode(c));
	}

	/**
		Appends a substring of [s] to [this] StringBuf.

		This function expects [pos] and [len] to describe a valid substring of
		[s], or else the result is unspecified. To get more robust behavior,
		[this].add(s.substr(pos,len)) can be used instead.

		If [s] or [pos] are null, the result is unspecified.

		If [len] is omitted or null, the substring ranges from [pos] to the end
		of [s].
	**/
	public inline function addSub( s : String, pos : Int, ?len : Int) : Void {
		add1((len == null ? s.substr(pos) : s.substr(pos, len)));
	}

	/**
		Returns the content of [this] StringBuf as String.

		The buffer is not emptied by this operation.
	**/
	public inline function toString() : String {
		return b.getvalue();
	}
}
