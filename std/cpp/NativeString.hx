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

package cpp;

extern class NativeString {
	static inline function raw(inString:String):RawConstPointer<Char> {
		return untyped inString.raw_ptr();
	}
	static inline function c_str(inString:String):ConstPointer<Char> {
		return cpp.ConstPointer.fromPointer(untyped inString.c_str());
	}
	static inline function fromPointer(inPtr:ConstPointer<Char>):String {
		return untyped __global__.String(inPtr.ptr);
	}
	static inline function fromGcPointer(inPtr:ConstPointer<Char>, inLen:Int):String {
		return untyped __global__.String(inPtr.ptr, inLen);
	}

	@:native("__hxcpp_parse_float")
	public static function parseFloat(inString:String):Float;

	@:native("__hxcpp_parse_substr_float")
	public static function parseSubstrFloat(inString:String,start:Int, length:Int):Float;

	// Will return 0 for invalid string
	@:native("__hxcpp_parse_substr_int")
	public static function parseInt(inString:String):Int;

	// Will return 0 for invalid string
	@:native("__hxcpp_parse_substr_int")
	public static function parseSubstrInt(inString:String,start:Int, length:Int):Int;

	@:native("_hx_string_compare")
	static function compare(inString0:String, inString1:String):Int;

	@:native("_hx_utf8_char_code_at")
	static function utf8CharCodeAt(inString:String, inIndex:Int):Int;

	@:native("_hx_utf8_length")
	static function utf8Length(inString:String):Int;

	@:native("_hx_utf8_is_valid")
	static function utf8IsValid(inString:String):Bool;

	@:native("_hx_utf8_sub")
	static function utf8Sub(inString:String, charStart:Int, inLen:Int):String;

	@:native("_hx_string_create")
	static function fromPointerLen(inPtr:ConstPointer<Char>, len:Int):String;

	@:native("_hx_utf8_decode_advance")
	static function utf8DecodeAdvance(reference:Char):Int;
}
