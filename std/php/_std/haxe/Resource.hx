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
import haxe.crypto.Base64;

@:coreApi
class Resource {

	static function cleanName(name : String) : String {
		return ~/[\\\/:?"*<>|]/.replace(name, '_');
	}

	static function getDir() : String {
		return untyped __call__('dirname', __php__('__FILE__'))+"/../../res";
	}

	@:access(haxe.io.Path.escape)
	static function getPath(name : String) : String {
		return getDir()+'/'+haxe.io.Path.escape(name);
	}

	@:access(haxe.io.Path.unescape)
	public static function listNames() : Array<String> {
		var a = sys.FileSystem.readDirectory(getDir());
		if(a[0] == '.') a.shift();
		if(a[0] == '..') a.shift();
		return a.map(function(s) return haxe.io.Path.unescape(s));
	}

	public static function getString( name : String ) : String {
		var path = getPath(name);
		return if (!sys.FileSystem.exists(path))
			null;
		else
			sys.io.File.getContent(path);
	}

	public static function getBytes( name : String ) : haxe.io.Bytes {
		var path = getPath(name);
		return if (!sys.FileSystem.exists(path))
			null;
		else
			sys.io.File.getBytes(path);
	}

}
