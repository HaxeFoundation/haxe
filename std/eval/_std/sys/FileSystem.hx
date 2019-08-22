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

package sys;

// This class is here so it re-shadows other FileSystem classes in macros,
// e.g. from hxnodejs.
@:coreApi
class FileSystem {
	extern static public function exists(path:String):Bool;

	extern static public function rename(path:String, newPath:String):Void;

	extern static public function stat(path:String):FileStat;

	extern static public function fullPath(relPath:String):String;

	static public function absolutePath(relPath:String):String {
		if (haxe.io.Path.isAbsolute(relPath))
			return relPath;
		return haxe.io.Path.join([Sys.getCwd(), relPath]);
	}

	extern static public function isDirectory(path:String):Bool;

	extern static public function createDirectory(path:String):Void;

	extern static public function deleteFile(path:String):Void;

	extern static public function deleteDirectory(path:String):Void;

	extern static public function readDirectory(path:String):Array<String>;
}
