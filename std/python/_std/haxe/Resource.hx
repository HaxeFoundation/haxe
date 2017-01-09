/*
 * Copyright (C)2005-2017 Haxe Foundation
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
package haxe;

import haxe.io.Bytes;
import haxe.io.BytesData;

@:coreApi class Resource {

	static var content:python.Dict<String,BytesData>;

	static function getContent():python.Dict<String,BytesData> {
		if (content == null) content = untyped _hx_resources__();
		return content;
	}

	public static inline function listNames() : Array<String> {
		return python.internal.UBuiltins.list(getContent().keys());
	}

	public static function getString( name : String ) : String {
		var bytes = getBytes(name);
		if (bytes != null)
			return bytes.toString();
		return null;
	}

	public static function getBytes( name : String ) : haxe.io.Bytes {
		var data = getContent().get(name, null);
		if (data == null)
			return null;
		return Bytes.ofData(data);
	}
}
