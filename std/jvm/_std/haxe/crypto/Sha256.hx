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
import haxe.io.BytesData;
import java.security.MessageDigest;
import java.nio.charset.StandardCharsets;

@:coreApi
class Sha256 {
	public static function encode(s:String):String {
		return Bytes.ofData(digest((cast s : java.NativeString).getBytes(StandardCharsets.UTF_8))).toHex();
	}

	public static function make(b:haxe.io.Bytes):haxe.io.Bytes {
		return Bytes.ofData(digest(b.getData()));
	}

	inline static function digest(b:BytesData):BytesData {
		return MessageDigest.getInstance("SHA-256").digest(b);
	}
}
