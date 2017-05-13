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

@:coreApi class Resource {

	@:keep static var content : Array<String>;
	static var paths : Map<String,String>;

	@:keep static function getPaths():Map<String,String> {
		if (paths != null)
			return paths;

		var p = new Map();
		var all = cs.Lib.toNativeType(haxe.Resource).Assembly.GetManifestResourceNames();
		for (i in 0...all.Length) {
			var path = all[i];
			var name = path.substr(path.indexOf("Resources.") + 10);
			p.set(name, path);
		}
		return paths = p;
	}

	public static inline function listNames() : Array<String> {
		return content.copy();
	}

	@:access(haxe.io.Path.escape)
	public static function getString( name : String ) : String {
		name = haxe.io.Path.escape(name, true);
		var path = getPaths().get(name);
		if (path == null)
			return null;
		var str = cs.Lib.toNativeType(haxe.Resource).Assembly.GetManifestResourceStream(path);
		if (str != null)
			return new cs.io.NativeInput(str).readAll().toString();
		return null;
	}

	@:access(haxe.io.Path.escape)
	public static function getBytes( name : String ) : haxe.io.Bytes {
		name = haxe.io.Path.escape(name, true);
		var path = getPaths().get(name);
		if (path == null)
			return null;
		var str = cs.Lib.toNativeType(haxe.Resource).Assembly.GetManifestResourceStream(path);
		if (str != null)
			return new cs.io.NativeInput(str).readAll();
		return null;
	}
}
