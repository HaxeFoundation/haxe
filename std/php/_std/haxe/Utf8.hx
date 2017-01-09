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

@:coreApi
class Utf8 {

	var __b : String;

	public function new( ?size : Int ) : Void {
		__b = '';
	}

	public function addChar( c : Int ) : Void {
		__b += uchr(c);
	}

	public function toString() : String {
		return __b;
	}

	public static function encode( s : String ) : String {
		return untyped __call__("utf8_encode", s);
	}

	public static function decode( s : String ) : String {
		return untyped __call__("utf8_decode", s);
	}

	public static function iter(s : String, chars : Int -> Void ) : Void {
		var len = length(s);
		for(i in 0...len)
			chars(charCodeAt(s, i));
	}

	public static function charCodeAt( s : String, index : Int ) : Int {
		return uord(sub(s, index, 1));
	}

	static function uchr(i : Int) : String {
		return untyped __php__("mb_convert_encoding(pack('N',$i), 'UTF-8', 'UCS-4BE')");
	}

	static function uord(s : String) : Int untyped {
		var c : Array<Int> = untyped __php__("unpack('N', mb_convert_encoding($s, 'UCS-4BE', 'UTF-8'))");
		return c[1];
	}

	public static function validate( s : String ) : Bool {
		return untyped __call__("mb_check_encoding", s, enc);
	}

	public static function length( s : String ) : Int {
		return untyped __call__("mb_strlen", s, enc);
	}

	public static function compare( a : String, b : String ) : Int {
		return untyped __call__("strcmp", a, b);
	}

	public static function sub( s : String, pos : Int, len : Int ) : String {
		return untyped __call__("mb_substr", s, pos, len, enc);
	}

	private static inline var enc = "UTF-8";
}