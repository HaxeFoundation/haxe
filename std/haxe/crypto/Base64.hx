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

/**
	Allows one to encode/decode String and bytes using Base64 encoding.
**/
class Base64 {
	public static var CHARS(default, null) = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";
	public static var BYTES(default, null) = haxe.io.Bytes.ofString(CHARS);

	public static var URL_CHARS(default, null) = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_";
	public static var URL_BYTES(default, null) = haxe.io.Bytes.ofString(URL_CHARS);

	public static function encode(bytes:haxe.io.Bytes, complement = true):String {
		var str = new BaseCode(BYTES).encodeBytes(bytes).toString();
		if (complement)
			switch (bytes.length % 3) {
				case 1:
					str += "==";
				case 2:
					str += "=";
				default:
			}
		return str;
	}

	public static function decode(str:String, complement = true):haxe.io.Bytes {
		if (complement)
			while (str.charCodeAt(str.length - 1) == "=".code)
				str = str.substr(0, -1);
		return new BaseCode(BYTES).decodeBytes(haxe.io.Bytes.ofString(str));
	}

	public static function urlEncode(bytes:haxe.io.Bytes, complement = false):String {
		var str = new BaseCode(URL_BYTES).encodeBytes(bytes).toString();
		if (complement)
			switch (bytes.length % 3) {
				case 1:
					str += "==";
				case 2:
					str += "=";
				default:
			}
		return str;
	}

	public static function urlDecode(str:String, complement = false):haxe.io.Bytes {
		if (complement)
			while (str.charCodeAt(str.length - 1) == "=".code)
				str = str.substr(0, -1);
		return new BaseCode(URL_BYTES).decodeBytes(haxe.io.Bytes.ofString(str));
	}
}
