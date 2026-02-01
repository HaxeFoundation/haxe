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

import haxe.io.Path;

@:coreApi
class FileSystem {
	public static function exists(path:String):Bool {
		return untyped __cs__("System.IO.File.Exists({0}) || System.IO.Directory.Exists({0})", path);
	}

	public static function rename(path:String, newPath:String):Void {
		try {
			if (isDirectory(path)) {
				untyped __cs__("System.IO.Directory.Move({0}, {1})", path, newPath);
			} else {
				untyped __cs__("System.IO.File.Move({0}, {1})", path, newPath);
			}
		} catch (e:Dynamic) {
			throw "Cannot rename " + path + " to " + newPath;
		}
	}

	public static function stat(path:String):FileStat {
		if (!exists(path))
			throw "Path " + path + " doesn't exist";

		var isDir = isDirectory(path);
		var size:Int = 0;
		var ctime:Float = 0;
		var atime:Float = 0;
		var mtime:Float = 0;

		if (isDir) {
			var dirInfo = untyped __cs__("new System.IO.DirectoryInfo({0})", path);
			ctime = untyped __cs__("(double)((System.DateTimeOffset){0}.CreationTimeUtc).ToUnixTimeMilliseconds()", dirInfo);
			atime = untyped __cs__("(double)((System.DateTimeOffset){0}.LastAccessTimeUtc).ToUnixTimeMilliseconds()", dirInfo);
			mtime = untyped __cs__("(double)((System.DateTimeOffset){0}.LastWriteTimeUtc).ToUnixTimeMilliseconds()", dirInfo);
		} else {
			var fileInfo = untyped __cs__("new System.IO.FileInfo({0})", path);
			size = untyped __cs__("(int){0}.Length", fileInfo);
			ctime = untyped __cs__("(double)((System.DateTimeOffset){0}.CreationTimeUtc).ToUnixTimeMilliseconds()", fileInfo);
			atime = untyped __cs__("(double)((System.DateTimeOffset){0}.LastAccessTimeUtc).ToUnixTimeMilliseconds()", fileInfo);
			mtime = untyped __cs__("(double)((System.DateTimeOffset){0}.LastWriteTimeUtc).ToUnixTimeMilliseconds()", fileInfo);
		}

		return {
			gid: 0,
			uid: 0,
			atime: Date.fromTime(atime),
			mtime: Date.fromTime(mtime),
			ctime: Date.fromTime(ctime),
			size: size,
			dev: 0,
			ino: 0,
			nlink: 0,
			rdev: 0,
			mode: 0
		};
	}

	public static function fullPath(relPath:String):String {
		return untyped __cs__("System.IO.Path.GetFullPath({0})", relPath);
	}

	public static function absolutePath(relPath:String):String {
		if (Path.isAbsolute(relPath))
			return relPath;
		return Path.join([Sys.getCwd(), relPath]);
	}

	public static function isDirectory(path:String):Bool {
		if (!exists(path))
			throw "Path " + path + " doesn't exist";
		return untyped __cs__("System.IO.Directory.Exists({0})", path);
	}

	public static function createDirectory(path:String):Void {
		try {
			untyped __cs__("System.IO.Directory.CreateDirectory({0})", path);
		} catch (e:Dynamic) {
			throw "Cannot create dir " + path;
		}
	}

	public static function deleteFile(path:String):Void {
		// C#'s File.Delete() silently does nothing if file doesn't exist
		// Haxe expects an exception to be thrown
		if (!untyped __cs__("System.IO.File.Exists({0})", path))
			throw "Cannot delete file " + path + " (file not found)";
		try {
			untyped __cs__("System.IO.File.Delete({0})", path);
		} catch (e:Dynamic) {
			throw "Cannot delete file " + path;
		}
	}

	public static function deleteDirectory(path:String):Void {
		try {
			untyped __cs__("System.IO.Directory.Delete({0})", path);
		} catch (e:Dynamic) {
			throw "Cannot delete directory " + path;
		}
	}

	public static function readDirectory(path:String):Array<String> {
		if (!exists(path))
			throw "Path " + path + " doesn't exist";

		var result = new Array<String>();
		var entries:Dynamic = untyped __cs__("System.IO.Directory.GetFileSystemEntries({0})", path);
		var length:Int = untyped __cs__("{0}.Length", entries);
		for (i in 0...length) {
			var entry:String = untyped __cs__("{0}[{1}]", entries, i);
			// Get just the file/directory name, not the full path
			var name:String = untyped __cs__("System.IO.Path.GetFileName({0})", entry);
			result.push(name);
		}
		return result;
	}
}
