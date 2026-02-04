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

import cs.system.io.DirectoryInfo;
import cs.system.io.File;
import cs.system.io.Directory;
import cs.system.io.FileInfo;

@:coreApi
class FileSystem {
	public static function exists(path:String):Bool {
		return (File.Exists(path) || Directory.Exists(path));
	}

	public static function rename(path:String, newPath:String):Void {
		if (isDirectory(path)) {
			Directory.Move(path, newPath);
		} else {
			File.Move(path, newPath);
		}
	}

	public static function stat(path:String):FileStat {
		if (File.Exists(path)) {
			var fi = new FileInfo(path);
			return {
				gid: 0,
				uid: 0,
				atime: dateFromNative(fi.LastAccessTime),
				mtime: dateFromNative(fi.LastWriteTime),
				ctime: dateFromNative(fi.CreationTime),
				size: cast(fi.Length, Int),
				dev: 0,
				ino: 0,
				nlink: 0,
				rdev: 0,
				mode: 0
			};
		} else if (Directory.Exists(path)) {
			var fi = new DirectoryInfo(path);
			return {
				gid: 0,
				uid: 0,
				atime: dateFromNative(fi.LastAccessTime),
				mtime: dateFromNative(fi.LastWriteTime),
				ctime: dateFromNative(fi.CreationTime),
				size: 0,
				dev: 0,
				ino: 0,
				nlink: 0,
				rdev: 0,
				mode: 0
			};
		} else {
			throw "Path '" + path + "' doesn't exist";
		}
	}

	private static function dateFromNative(native:cs.system.DateTime):Date {
		// Convert .NET DateTime to Haxe Date via Unix timestamp
		var ticks:haxe.Int64 = untyped __cs__("{0}.ToUniversalTime().Ticks", native);
		// .NET ticks are 100-nanosecond intervals since 0001-01-01
		// Unix epoch is 1970-01-01, difference is 621355968000000000 ticks
		var unixTicks:haxe.Int64 = haxe.Int64.sub(ticks, haxe.Int64.make(0x89F7FF5F, 0x7B58000));
		// Convert to milliseconds
		var ms:Float = haxe.Int64.toInt(haxe.Int64.div(unixTicks, haxe.Int64.ofInt(10000)));
		return Date.fromTime(ms);
	}

	public static function fullPath(relPath:String):String {
		return new FileInfo(relPath).FullName;
	}

	public static function absolutePath(relPath:String):String {
		if (haxe.io.Path.isAbsolute(relPath))
			return relPath;
		return haxe.io.Path.join([Sys.getCwd(), relPath]);
	}

	public static function isDirectory(path:String):Bool {
		var isdir = Directory.Exists(path);
		if (isdir != File.Exists(path))
			return isdir;
		throw "Path '" + path + "' doesn't exist";
	}

	public static function createDirectory(path:String):Void {
		Directory.CreateDirectory(path);
	}

	public static function deleteFile(path:String):Void {
		if (!File.Exists(path))
			throw "Path '" + path + "' doesn't exist";
		File.Delete(path);
	}

	public static function deleteDirectory(path:String):Void {
		if (!Directory.Exists(path))
			throw "Path '" + path + "' doesn't exist";
		Directory.Delete(path);
	}

	public static function readDirectory(path:String):Array<String> {
		var native = Directory.GetFileSystemEntries(path);
		var result = new Array<String>();
		if (native.length > 0) {
			var fst = native[0];
			var sep = "/";
			if (fst.lastIndexOf(sep) < fst.lastIndexOf("\\"))
				sep = "\\";
			for (i in 0...native.length) {
				var p = native[i];
				result.push(p.substr(p.lastIndexOf(sep) + 1));
			}
		}
		return result;
	}
}
