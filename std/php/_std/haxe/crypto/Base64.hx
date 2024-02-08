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

import php.Global.*;
import php.Syntax;
import php.NativeArray;
import haxe.io.Bytes;

class Base64 {
	public static var CHARS(default, null) = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";
	public static var BYTES(default, null) = haxe.io.Bytes.ofString(CHARS);

	public static var URL_CHARS(default, null) = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_";
	public static var URL_BYTES(default, null) = haxe.io.Bytes.ofString(URL_CHARS);

	static final NORMAL_62_63:NativeArray = Syntax.arrayDecl('+', '/');
	static final URL_62_63:NativeArray = Syntax.arrayDecl('-', '_');

	public static inline function encode(bytes:Bytes, complement = true):String {
		var result = base64_encode(bytes.toString());
		return (complement ? result : rtrim(result, "="));
	}

	public static inline function decode(str:String, complement = true):Bytes {
		if (!complement) {
			switch (strlen(str) % 4) {
				case 2:
					str += "==";
				case 3:
					str += "=";
				default:
			}
		}
		return switch base64_decode(str, true) {
			case false: throw new Exception("Base64.decode : invalid encoded string");
			case s: Bytes.ofString(s);
		}
	}

	public static inline function urlEncode(bytes:Bytes, complement = false):String {
		var result = str_replace(NORMAL_62_63, URL_62_63, base64_encode(bytes.toString()));
		return (complement ? result : rtrim(result, "="));
	}

	public static inline function urlDecode(str:String, complement = false):Bytes {
		if (complement) {
			switch (strlen(str) % 4) {
				case 2:
					str += "==";
				case 3:
					str += "=";
				default:
			}
		}
		return switch base64_decode(str_replace(URL_62_63, NORMAL_62_63, str), true) {
			case false: throw new Exception("Base64.urlDecode : invalid encoded string");
			case s: Bytes.ofString(s);
		}
	}
}
