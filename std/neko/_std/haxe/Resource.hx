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

package haxe;

@:coreApi
class Resource {
	static var content:Array<{name:String, data:String, str:String}>;

	public static function listNames():Array<String> {
		return [for (x in content) x.name];
	}

	public static function getString(name:String):String {
		for (x in content)
			if (x.name == name) {
				return new String(x.data);
			}
		return null;
	}

	public static function getBytes(name:String):haxe.io.Bytes {
		for (x in content)
			if (x.name == name) {
				return haxe.io.Bytes.ofData(cast x.data);
			}
		return null;
	}

	static function __init__() : Void {
		var tmp = untyped __resources__();
		content = untyped Array.new1(tmp, __dollar__asize(tmp));
	}
}
