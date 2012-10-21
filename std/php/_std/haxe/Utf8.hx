/*
 * Copyright (c) 2005, The haXe Project Contributors
 * All rights reserved.
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 *   - Redistributions of source code must retain the above copyright
 *     notice, this list of conditions and the following disclaimer.
 *   - Redistributions in binary form must reproduce the above copyright
 *     notice, this list of conditions and the following disclaimer in the
 *     documentation and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE HAXE PROJECT CONTRIBUTORS "AS IS" AND ANY
 * EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE HAXE PROJECT CONTRIBUTORS BE LIABLE FOR
 * ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
 * SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
 * CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH
 * DAMAGE.
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