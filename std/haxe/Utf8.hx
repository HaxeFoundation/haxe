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
package haxe;

/**
	Since not all platforms guarantee that `String` always uses UTF-8 encoding, you
	can use this cross-platform API to perform operations on such strings.
**/
class Utf8 {

	var __b : String;

	/**
		Allocate a new Utf8 buffer using an optional bytes size.
	**/
	public function new( ?size : Int ) {
		__b = "";
	}

	/**
		Add the given UTF8 character code to the buffer.
	**/
	public inline function addChar( c : Int ) : Void {
		__b += String.fromCharCode(c);
	}

	/**
		Returns the buffer converted to a String.
	**/
	public inline function toString() : String {
		return __b;
	}

	/**
		Call the `chars` function for each UTF8 char of the string.
	**/
	public static function iter( s : String, chars : Int -> Void ) {
		for( i in 0...s.length )
			chars(s.charCodeAt(i));
	}

	/**
		Encode the input ISO string into the corresponding UTF8 one.
	**/
	public static function encode( s : String ) : String {
		throw "Not implemented";
	}

	/**
		Decode an UTF8 string back to an ISO string.
		Throw an exception if a given UTF8 character is not supported by the decoder.
	**/
	public static function decode( s : String ) : String {
		throw "Not implemented";
	}

	/**
		Similar to `String.charCodeAt` but uses the UTF8 character position.
	**/
	public static inline function charCodeAt( s : String, index : Int ) : Int {
		return s.charCodeAt(index);
	}

	/**
		Tells if the String is correctly encoded as UTF8.
	**/
	public static inline function validate( s : String ) : Bool {
		return true;
	}

	/**
		Returns the number of UTF8 chars of the String.
	**/
	#if js @:extern #end
	public static inline function length( s : String ) : Int {
		return s.length;
	}

	/**
		Compare two UTF8 strings, character by character.
	**/
	public static function compare( a : String, b : String ) : Int {
		return a > b ? 1 : (a == b ? 0 : -1);
	}

	/**
		This is similar to `String.substr` but the `pos` and `len` parts are considering UTF8 characters.
	**/
	public static inline function sub( s : String, pos : Int, len : Int ) : String {
		return s.substr(pos,len);
	}

}
