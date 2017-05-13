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

import lua.Io;
import lua.Os;
import lua.Lib;
import lua.Table;
import haxe.io.Path;
typedef LFileSystem = lua.lib.luv.fs.FileSystem;

class FileSystem {
	public static function exists( path : String ) : Bool {
		if (path == null) return false;
		else{
			var f = Io.open(path);
			if (f == null) return false;
			else {
				f.close();
				return true;
			}
		}
	}

	public inline static function rename( path : String, newPath : String ) : Void {
		var ret = Os.rename(path, newPath);
		if (!ret.success){
			throw ret.message;
		}
	}

	public inline static function stat( path : String ) : FileStat {
		var ls =  LFileSystem.stat(path);
		if (ls.result == null) throw ls.message;
		var l = ls.result;
		return {
			gid   : l.gid,
			uid   : l.uid,
			rdev  : l.rdev,
			size  : l.size,
			nlink : l.nlink,
			mtime : Date.fromTime(l.mtime.sec + l.mtime.nsec/1000000),
			mode  : l.mode,
			ino   : l.ino,
			dev   : l.dev,
			ctime : Date.fromTime(l.ctime.sec + l.ctime.nsec/1000000),
			atime : Date.fromTime(l.atime.sec + l.atime.nsec/1000000)
		};
	}

	public inline static function fullPath( relPath : String ) : String {
		return Path.normalize(absolutePath(relPath));
	}

	public inline static function absolutePath( relPath : String ) : String {
		var pwd = lua.lib.luv.Misc.cwd();
		if (pwd == null) return relPath;
		return Path.join([pwd, relPath]);
	}

	public inline static function deleteFile( path : String ) : Void {
		var ret = lua.Os.remove(path);
		if (!ret.success){
			throw ret.message;
		}
	}

	public inline static function readDirectory( path : String ) : Array<String> {
		var scandir = LFileSystem.scandir(path);

		var itr = function() {
			var next = LFileSystem.scandir_next(scandir).name;
			return next;
		}
		return lua.Lib.fillArray(itr);
	}

	public inline static function isDirectory( path : String ) : Bool {
		var result = LFileSystem.stat(path).result;
		if (result == null) return false;
		else return result.type ==  "directory";
	}

	public inline static function deleteDirectory( path : String ) : Void {
		var ret = LFileSystem.rmdir(path);
		if (ret.result == null){
			throw ret.message;
		}
	}

	public inline static function createDirectory( path : String ) : Void {

		var path = haxe.io.Path.addTrailingSlash(path);
		var _p = null;
		var parts = [];
		while (path != (_p = haxe.io.Path.directory(path))) {
			parts.unshift(path);
			path = _p;
		}
		for (part in parts) {
			if (part.charCodeAt(part.length - 1) != ":".code && !exists(part) && !LFileSystem.mkdir( part, 511 ).result)
				throw "Could not create directory:" + part;
		}
	}
}
