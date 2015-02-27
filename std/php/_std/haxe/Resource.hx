/*
 * Copyright (C)2005-2012 Haxe Foundation
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

	private static function unescapeName( name : String ) : String
	{
		var regex = ~/-x([0-9]{2})/g;
		return regex.map(name, function(regex) return String.fromCharCode(Std.parseInt(regex.matched(1))));
	}

	private static function escapeName( name : String ) : String {
		var regex = ~/[^A-Za-z0-9_]/g;
		return regex.map(name, function(v) return '-x' + v.matched(0).charCodeAt(0));
	}

	static function getPath(name : String) : String {
		return getDir()+'/'+escapeName(name);
	}

	public static function listNames() : Array<String> {
		var a = sys.FileSystem.readDirectory(getDir());
		if(a[0] == '.') a.shift();
		if(a[0] == '..') a.shift();
		return a.map(function(s) return unescapeName(s));
	}

	public static function getString( name : String ) : String {
		return sys.io.File.getContent(getPath(name));
	}

	public static function getBytes( name : String ) : haxe.io.Bytes {
		return sys.io.File.getBytes(getPath(name));
	}

}
