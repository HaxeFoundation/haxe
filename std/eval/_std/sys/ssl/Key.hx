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

import haxe.io.Bytes;
import mbedtls.PkContext;

@:coreApi
class Key {
	var native:PkContext;

	function new() {
		native = new PkContext();
	}

	static public function loadFile(file:String, ?isPublic:Bool, ?pass:String):Key {
		var key = new Key();
		var code = if (isPublic) {
			key.native.parse_public_keyfile(file);
		} else {
			key.native.parse_keyfile(file, pass);
		}
		if (code != 0) {
			throw(mbedtls.Error.strerror(code));
		}
		return key;
	}

	static function parse(data:Bytes, isPublic:Bool, ?pass:String):Key {
		var key = new Key();
		var code = if (isPublic) {
			key.native.parse_public_key(data);
		} else {
			key.native.parse_key(data);
		}
		if (code != 0) {
			throw(mbedtls.Error.strerror(code));
		}
		return key;
	}

	static public function readPEM(data:String, isPublic:Bool, ?pass:String):Key {
		return parse(Bytes.ofString(data), isPublic, pass);
	}

	static public function readDER(data:haxe.io.Bytes, isPublic:Bool):Key {
		return parse(data, isPublic);
	}
}
