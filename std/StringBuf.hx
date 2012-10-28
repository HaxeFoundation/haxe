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
/**
	A String buffer is an efficient way to build a big string by
	appending small elements together.
**/
class StringBuf {

	var b:String = "";
	
	/**
		Creates a new string buffer.
	**/
	public function new() {}

	/**
		Adds the representation of any value to the string buffer.
	**/
	public inline function add( x : Dynamic ) : Void {
		b += x;
	}

	/**
		Adds a part of a string to the string buffer.
	**/
	public inline function addChar( c : Int ) : Void {
		b += String.fromCharCode(c);
	}

	/**
		Adds a character to the string buffer.
	**/
	public inline function addSub( s : String, pos : Int, ?len : Int) : Void {
		b += s.substr(pos, len);
	}

	/**
		Returns the content of the string buffer.
		The buffer is not emptied by this operation.
	**/
	public inline function toString() : String {
		return b;
	}

}
