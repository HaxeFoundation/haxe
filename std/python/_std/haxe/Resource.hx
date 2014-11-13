/*
 * Copyright (C)2005-2014 Haxe Foundation
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

	static var content : python.lib.Dict<String, BytesData> = untyped _hx_resources__();

	public static inline function listNames() : Array<String> {
		return python.lib.Builtin.list(content.keys());
	}

	public static function getString( name : String ) : String {
        #if embed_resources
		for (k in content.keys().iter()) {
			if (k == name) {
				var b : haxe.io.Bytes = haxe.crypto.Base64.decode(content.get(k, null));
				return b.toString();
			}
		}
		return null;
        #else
        return content.hasKey(name) ? Bytes.ofData(content.get(name, null)).toString() : null;
        #end
	}

	public static function getBytes( name : String ) : haxe.io.Bytes {
        #if embed_resources
		for( k in content.keys().iter() )
			if( k == name ) {
				var b : haxe.io.Bytes = haxe.crypto.Base64.decode(content.get(k, null));
				return b;

			}
        #else
        return Bytes.ofData(content.get(name,null));
        #end
	}
}
