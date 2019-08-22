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

import cpp.NativeSsl;

private typedef PKEY = Dynamic;

@:coreApi
class Key {
	private var __k:PKEY;

	private function new(k:PKEY) {
		__k = k;
	}

	public static function loadFile(file:String, ?isPublic:Bool, ?pass:String):Key {
		var data = sys.io.File.getBytes(file);
		var str = cpp.Lib.stringReference(data);
		if (str.indexOf("-----BEGIN ") >= 0)
			return readPEM(str, isPublic == true, pass);
		else
			return readDER(data, isPublic == true);
	}

	public static function readPEM(data:String, isPublic:Bool, ?pass:String):Key {
		return new Key(NativeSsl.key_from_pem(data, isPublic, pass));
	}

	public static function readDER(data:haxe.io.Bytes, isPublic:Bool):Key {
		return new Key(NativeSsl.key_from_der(data.getData(), isPublic));
	}

	static function __init__():Void {
		NativeSsl.init();
	}
}
