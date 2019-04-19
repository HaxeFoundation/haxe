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
package haxe;

import haxe.io.Bytes;
import haxe.io.Encoding;

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
		for(c in new haxe.iterators.StringIteratorUnicode(s)) {
			chars(c);
		}
	}

	/**
		Encode the input string into the corresponding UTF8 byte sequence.
	**/
	public static inline function encode( s : String ) : Bytes {
		return Bytes.ofString(s, Encoding.UTF8);
	}

	/**
		Decode an UTF8 byte sequence back to a string.
		Throw an exception if a given UTF8 character is not supported by the decoder.
	**/
	public static inline function decode( b : Bytes ) : String {
		return b.getString(0, b.length, Encoding.UTF8);
	}

	/**
		Similar to `String.charCodeAt` but uses the UTF8 character position.
	**/
	#if !utf16 inline #end
	public static function charCodeAt( s : String, index : Int ) : Int {
		#if utf16
			for(i => c in new haxe.iterators.StringKeyValueIteratorUnicode(s)) {
				if(i == index) return c;
			}
			return 0;
		#else
			return s.charCodeAt(index);
		#end
	}

	/**
		Tells if `b` is a correctly encoded UTF8 byte sequence.
	**/
	public static function validate( b : Bytes ) : Bool {
		var data = b.getData();
		var pos = 0;
		var max = b.length;
		while( pos < max) {
			var c:Int = Bytes.fastGet(data, pos++);
			if(c < 0x80) {
			} else if(c < 0xC2) {
				return false;
			} else if(c < 0xE0) {
				if(pos + 1 > max) {
					return false;
				}
				var c2:Int = Bytes.fastGet(data, pos++);
				if(c2 < 0x80 || c2 > 0xBF) {
					return false;
				}
			} else if(c < 0xF0) {
				if(pos + 2 > max) {
					return false;
				}
				var c2:Int = Bytes.fastGet(data, pos++);
				if(c == 0xE0) {
					if(c2 < 0xA0 || c2 > 0xBF) return false;
				} else {
					if(c2 < 0x80 || c2 > 0xBF) return false;
				}
				var c3:Int = Bytes.fastGet(data, pos++);
				if(c3 < 0x80 || c3 > 0xBF) {
					return false;
				}
				c = (c << 16) | (c2 << 8) | c3;
				if(0xEDA080 <= c && c <= 0xEDBFBF) { //surrogate pairs
					return false;
				}
			} else if(c > 0xF4) {
				return false;
			} else {
				if(pos + 3 > max) {
					return false;
				}
				var c2:Int = Bytes.fastGet(data, pos++);
				if(c == 0xF0) {
					if(c2 < 0x90 || c2 > 0xBF) return false;
				} else if(c == 0xF4) {
					if(c2 < 0x80 || c2 > 0x8F) return false;
				} else {
					if(c2 < 0x80 || c2 > 0xBF) return false;
				}
				var c3:Int = Bytes.fastGet(data, pos++);
				if(c3 < 0x80 || c3 > 0xBF) {
					return false;
				}
				var c4:Int = Bytes.fastGet(data, pos++);
				if(c4 < 0x80 || c4 > 0xBF) {
					return false;
				}
			}
		}
		return true;
	}

	/**
		Returns the number of UTF8 chars of the String.
	**/
	#if !utf16 inline #end
	public static function length( s : String ) : Int {
		#if utf16
			var result = 0;
			for(i in 0...s.length) {
				result++;
				var c = StringTools.fastCodeAt(s, i);
				if (0xD800 <= c && c <= 0xDBFF) {
					result--;
				}
			}
			return result;
		#else
			return s.length;
		#end
	}

	/**
		This is similar to `String.substr` but the `pos` and `len` parts are considering UTF8 characters.
	**/
	#if !utf16 inline #end
	public static function sub( s : String, pos : Int, len : Int ) : String {
		#if utf16
			if(pos < 0) {
				pos = length(s) + pos;
			}
			var result = '';
			var byteOffset = 0;
			var charOffset = 0;
			var firstByte = 0;
			var toByte = 0;
			var toPos = pos + len;
			do {
				if(charOffset == pos) {
					firstByte = byteOffset;
				}
				if(charOffset >= toPos || byteOffset >= s.length) {
					toByte = charOffset;
					break;
				}
				var c = StringTools.fastCodeAt(s, byteOffset++);
				if (c >= 0xD800 && c < 0xDBFF) {
					byteOffset++;
				}
				charOffset++;
			} while(true);
			return s.substring(firstByte, toByte);
		#else
			return s.substr(pos,len);
		#end
	}

}
