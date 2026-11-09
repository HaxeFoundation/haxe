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

package haxe.crypto;

import haxe.io.Bytes;
import python.Syntax;

/**
	Allows one to encode/decode String and bytes using Base64 encoding.

	On Python this uses the base64 module: the generic version, a loop over the bytes, takes
	minutes for a megabyte. Results and errors are those of the generic version.
**/
class Base64 {
	public static var CHARS(default, null) = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";
	public static var BYTES(default, null) = haxe.io.Bytes.ofString(CHARS);

	public static var URL_CHARS(default, null) = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_";
	public static var URL_BYTES(default, null) = haxe.io.Bytes.ofString(URL_CHARS);

	public static function encode(bytes:Bytes, complement = true):String {
		var str:String = Syntax.code("{0}.decode('ascii')", NativeBase64.b64encode(bytes.getData()));
		return complement ? str : Syntax.code("{0}.rstrip('=')", str);
	}

	public static function decode(str:String, complement = true):Bytes {
		return decodeWith(str, complement, CHARS, null);
	}

	public static function urlEncode(bytes:Bytes, complement = false):String {
		var str:String = Syntax.code("{0}.decode('ascii')", NativeBase64.urlsafe_b64encode(bytes.getData()));
		return complement ? str : Syntax.code("{0}.rstrip('=')", str);
	}

	public static function urlDecode(str:String, complement = false):Bytes {
		return decodeWith(str, complement, URL_CHARS, Syntax.code("b'-_'"));
	}

	/**
		As the generic version decodes: with `complement`, trailing "=" are removed first; then every
		char must be of `chars` ("BaseCode : invalid encoded char" otherwise), and bits that don't
		make a whole byte (a single char left over) are ignored.
	**/
	static function decodeWith(str:String, complement:Bool, chars:String, altchars:Dynamic):Bytes {
		if (complement)
			str = Syntax.code("{0}.rstrip('=')", str);
		if (Syntax.code("not set({0}).issubset({1})", str, chars))
			throw "BaseCode : invalid encoded char";
		var whole = str.length - str.length % 4;
		var rest = str.length % 4;
		var padded = str.substr(0, rest == 1 ? whole : str.length) + (rest == 2 ? "==" : rest == 3 ? "=" : "");
		return Bytes.ofData(Syntax.code("bytearray({0})", NativeBase64.b64decode(padded, altchars)));
	}
}

@:pythonImport("base64")
private extern class NativeBase64 {
	static function b64encode(s:Dynamic):Dynamic;
	static function urlsafe_b64encode(s:Dynamic):Dynamic;
	static function b64decode(s:String, ?altchars:Dynamic):Dynamic;
}
