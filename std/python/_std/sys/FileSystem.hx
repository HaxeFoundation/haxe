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
package sys;

import python.lib.Os;
import python.lib.os.Path;

@:coreApi
class FileSystem {

	public static function exists( path : String ) : Bool {
		return Path.exists(path);
	}

	public static function stat( path : String ) : sys.FileStat {
		var s = Os.stat(path);
		return {
			gid : s.st_gid,
			uid : s.st_uid,
			atime : Date.fromTime(s.st_atime),
			mtime : Date.fromTime(s.st_mtime),
			ctime : Date.fromTime(s.st_ctime),
			size : s.st_size,
			dev : s.st_dev,
			ino : s.st_ino,
			nlink : s.st_nlink,
			rdev : python.internal.UBuiltins.getattr(s, "st_rdev", 0), // st_rdev is not available on Windows
			mode : s.st_mode
		}
	}

	public static function rename( path : String, newPath : String ) : Void {
		Os.rename(path, newPath);
	}

	public static function fullPath( relPath : String ) : String {
		return Path.realpath(relPath);
	}

	public static function absolutePath ( relPath : String ) : String {
		if (haxe.io.Path.isAbsolute(relPath)) return relPath;
		return haxe.io.Path.join([Sys.getCwd(), relPath]);
	}

	public static function isDirectory( path : String ) : Bool
	{
		return Path.isdir(path);
	}

	public static function createDirectory( path : String ) : Void {
		Os.makedirs(path, 511, true);
	}

	public static function deleteFile( path : String ) : Void {
		Os.remove(path);
	}

	public static function deleteDirectory( path : String ) : Void {
		Os.rmdir(path);
	}

	public static function readDirectory( path : String ) : Array<String> {
		return Os.listdir(path);
	}

}
