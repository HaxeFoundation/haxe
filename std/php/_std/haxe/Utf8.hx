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

import php.Global;
import haxe.io.Bytes;

using haxe.iterators.StringIteratorUnicode;


@:coreApi
class Utf8 {
	static inline var ENC_UTF8 = "UTF-8";

	var __b : String;

	public function new( ?size : Int ) : Void {
		__b = '';
	}

	public function addChar( c : Int ) : Void {
		__b += Global.mb_chr(c);
	}

	public function toString() : String {
		return __b;
	}

	public static function encode( s : String ) : Bytes {
		return Bytes.ofString(Global.utf8_encode(s));
	}

	public static function decode( b : Bytes ) : String {
		return b.toString();
	}

	public static function iter(s : String, chars : Int -> Void ) : Void {
		for(c in s.unicodeIterator()) {
			chars(c);
		}
	}

	public static inline function charCodeAt( s : String, index : Int ) : Int {
		return s.charCodeAt(index);
	}

	public static inline function validate( b : Bytes ) : Bool {
		return Global.mb_check_encoding(b.toString(), ENC_UTF8);
	}

	public static inline function length( s : String ) : Int {
		return Global.mb_strlen(s, ENC_UTF8);
	}

	public static inline function sub( s : String, pos : Int, len : Int ) : String {
		return Global.mb_substr(s, pos, len, ENC_UTF8);
	}
}