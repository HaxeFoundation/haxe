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

package sys.ssl;

import sys.ssl.Lib;

@:noDoc
typedef KeyPtr = hl.Abstract<"hl_ssl_pkey">;

@:coreApi
class Key {
	private var __k:KeyPtr;

	private function new(k:KeyPtr) {
		__k = k;
	}

	public static function loadFile(file:String, ?isPublic:Bool, ?pass:String):Key {
		var data = sys.io.File.getBytes(file);
		var start = data.getString(0, 11);
		if (start == "-----BEGIN ")
			return readPEM(data.toString(), isPublic == true, pass);
		else
			return readDER(data, isPublic == true);
	}

	public static function readPEM(data:String, isPublic:Bool, ?pass:String):Key {
		return new Key(key_from_pem(@:privateAccess data.toUtf8(), isPublic, pass == null ? null : @:privateAccess pass.toUtf8()));
	}

	public static function readDER(data:haxe.io.Bytes, isPublic:Bool):Key {
		return new Key(key_from_der(@:privateAccess data.b, @:privateAccess data.length, isPublic));
	}

	@:hlNative("ssl", "key_from_pem") static function key_from_pem(data:hl.Bytes, pub:Bool, pass:Null<hl.Bytes>):KeyPtr {
		return null;
	}

	@:hlNative("ssl", "key_from_der") static function key_from_der(data:hl.Bytes, len:Int, pub:Bool):KeyPtr {
		return null;
	}
}
