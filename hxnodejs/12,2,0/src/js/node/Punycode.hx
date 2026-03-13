/*
 * Copyright (C)2014-2020 Haxe Foundation
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

package js.node;

@:deprecated("In a future major version of Node.js punycode module will be removed")
@:jsRequire("punycode")
extern class Punycode {
	/**
		Converts a Punycode string of ASCII code points to a string of Unicode code points.
	**/
	static function decode(string:String):String;

	/**
		Converts a string of Unicode code points to a Punycode string of ASCII code points.
	**/
	static function encode(string:String):String;

	/**
		Converts a Punycode string representing a domain name to Unicode.

		Only the Punycoded parts of the domain name will be converted, i.e. it doesn't matter
		if you call it on a string that has already been converted to Unicode.
	**/
	static function toUnicode(domain:String):String;

	/**
		Converts a Unicode string representing a domain name to Punycode.

		Only the non-ASCII parts of the domain name will be converted, i.e. it doesn't matter
		if you call it with a domain that's already in ASCII.
	**/
	static function toASCII(domain:String):String;

	static var ucs2(default, null):PunycodeUcs2;

	/**
		The current Punycode.js version number.
	**/
	static var version(default, null):String;
}

@:deprecated
extern class PunycodeUcs2 {
	/**
		Creates an array containing the decimal code points of each Unicode character in the string.
		While JavaScript uses UCS-2 internally, this function will convert a pair of surrogate halves (each of which
		UCS-2 exposes as separate characters) into a single code point, matching UTF-16.
	**/
	function decode(string:String):Array<Int>;

	/**
		Creates a string based on an array of decimal code points.
	**/
	function encode(codePoints:Array<Int>):String;
}
