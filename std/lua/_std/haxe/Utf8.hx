/*
 * Copyright (C)2005-2016 Haxe Foundation
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

import lua.NativeStringTools;

/**
  A Lua-specific implementation of Utf8, using a helper library.
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
		__b += char(c);
	}

	/**
		Returns the buffer converted to a String;
	**/
	public inline function toString() : String {
		return __b;
	}

	/**
		Call the `chars` function for each UTF8 char of the string.
	**/
	public static function iter( s : String, chars : Int -> Void ) {
		for( i in 0...s.length ) chars(s.charCodeAt(i));
	}

	/**
		Encode the input ISO string into the corresponding UTF8 one.
	**/
	public static function encode( s : String ) : String {
		throw "Unimplemented";
	}

	/**
		Decode an UTF8 string back to an ISO string.
		Throw an exception if a given UTF8 character is not supported by the decoder.
	**/
	public static function decode( s : String ) : String {
		throw "Unimplemented";
	}

	/**
		Similar to `String.charCodeAt` but uses the UTF8 character position.
	**/
	public static inline function charCodeAt( s : String, index : Int ) : Int {
		var cur_idx = 0;
		var pos = 0;
		for (i in 0...index){
			pos += charWidth(s.charCodeAt(pos));
		}
		var ret = 0;
		var code = s.charCodeAt(pos);
		var bytes = charWidth(code);
		if (bytes == 1){
			return code;
		} else if (bytes == 2){
			return ((code & 0x1F) << 6) | (s.charCodeAt(pos+1) & 0x3F);
		} else if (bytes == 3){
			return ((code & 0x0F) << 12) | (((s.charCodeAt(pos+1) & 0x3F) << 6) | (s.charCodeAt(pos+2) & 0x3F));
		} else {
			return null;
		}
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
	public static inline function length( s : String ) : Int {
		var pos = 0;
		var len = 0;
		while (pos < s.length){
			pos += charWidth(s.charCodeAt(pos));
			len++;
		}
		return len;
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
		var startpos = 0;
		var ret = new StringBuf();
		for (i in 0...pos){
			startpos += charWidth(s.charCodeAt(startpos));
		}
		var endpos = startpos;
		for (i in 0...len){
			endpos += charWidth(s.charCodeAt(endpos));
		}
		return s.substring(startpos, endpos);
	}

	private static function charWidth(c:Int) : Int {
		return   if (c >  0   && c <= 127) 1;
			else if (c >= 194 && c <= 223) 2;
			else if (c >= 224 && c <= 239) 3;
			else if (c >= 240 && c <= 244) 4;
			else null;
	}

	private static function char( unicode : Int ) : String {
		if (unicode <= 0x7F) {
			return String.fromCharCode(unicode);
		} else if (unicode <= 0x7FF) {
			var b0 = 0xC0 + Math.floor(unicode / 0x40);
			var b1 = 0x80 + (unicode % 0x40);
			return NativeStringTools.char(b0, b1);
		} else if (unicode <= 0xFFFF) {
			var b0 = 0xE0 +  Math.floor(unicode / 0x1000);
			var b1 = 0x80 + (Math.floor(unicode / 0x40) % 0x40);
			var b2 = 0x80 + (unicode % 0x40);
			return NativeStringTools.char(b0, b1, b2);
		} else if (unicode <= 0x10FFFF) {
			var code = unicode;
			var b3   = 0x80 + (code % 0x40);
			code     = Math.floor(code / 0x40);
			var b2   = 0x80 + (code % 0x40);
			code     = Math.floor(code / 0x40);
			var b1   = 0x80 + (code % 0x40);
			code     = Math.floor(code / 0x40);
			var b0   = 0xF0 + code;

			return NativeStringTools.char(b0, b1, b2, b3);
		} else {
			throw 'Unicode greater than U+10FFFF';
		}
	}
	static var iso2uni = [ 8364, 65533, 8218, 402, 8222, 8230, 8224, 8225, 710, 8240, 352, 8249, 338, 65533, 381, 65533, 65533, 8216, 8217, 8220, 8221, 8226, 8211, 8212, 732, 8482, 353, 8250, 339, 65533, 382, 376];
}
