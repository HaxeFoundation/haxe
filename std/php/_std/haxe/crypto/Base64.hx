/*
 * Copyright (C)2005-2018 Haxe Foundation
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

import php.Global;
import haxe.io.Bytes;

class Base64 {

	public static var CHARS(default,null) = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";
	public static var BYTES(default,null) = haxe.io.Bytes.ofString(CHARS);

	public static inline function encode( bytes : Bytes, complement = true ) : String {
		var result = Global.base64_encode( bytes.toString() );
		return (complement ? result : Global.rtrim(result, "="));
	}

	public static inline function decode( str : String, complement = true ) : Bytes {
		if(!complement) {
			switch (Global.strlen(str) % 3) {
				case 1:
					str += "==";
				case 2:
					str += "=";
				default:
			}
		}
		return Bytes.ofString( Global.base64_decode( str, true ) );
	}
}
