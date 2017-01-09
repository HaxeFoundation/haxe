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

import php.*;
import haxe.io.Path;

private enum FileKind {
	kdir;
	kfile;
	kother( k : String );
}

@:coreApi
class FileSystem {

	public static inline function exists( path : String ) : Bool {
		return Global.file_exists(path);
	}

	public static inline function rename( path : String, newPath : String ) : Void {
		Global.rename(path, newPath);
	}

	public static function stat( path : String ) : FileStat {
		var info = Global.stat(path);
		if (info == false) throw 'Unable to stat $path';
		var info:NativeArray = info;

		return {
			gid   : info['gid'],
			uid   : info['uid'],
			atime : Date.fromTime(info['atime'] * 1000),
			mtime : Date.fromTime(info['mtime'] * 1000),
			ctime : Date.fromTime(info['ctime'] * 1000),
			dev   : info['dev'],
			ino   : info['ino'],
			nlink : info['nlink'],
			rdev  : info['rdev'],
			size  : info['size'],
			mode  : info['mode']
		};
	}

	public static inline function fullPath( relPath : String ) : String {
		return (Syntax.binop(Global.realpath(relPath), "?:", null));
	}

	public static function absolutePath ( relPath : String ) : String {
		if (Path.isAbsolute(relPath)) return relPath;
		return Path.join([Sys.getCwd(), relPath]);
	}

	static function kind( path : String ) : FileKind {
		var kind = Global.filetype(path);
		if (kind == false) throw 'Failed to check file type $path';

		switch(kind) {
			case "file": return kfile;
			case "dir": return kdir;
			default: return kother(kind);
		}
	}

	public static inline function isDirectory( path : String ) : Bool {
		return Global.is_dir(path);
	}

	public static inline function createDirectory( path : String ) : Void {
		Global.mkdir(path, 493, true);
	}

	public static inline function deleteFile( path : String ) : Void {
		Global.unlink(path);
	}

	public static inline function deleteDirectory( path : String ) : Void {
		Global.rmdir(path);
	}

	public static function readDirectory( path : String ) : Array<String> {
		var list = [];
		var dir = Global.opendir(path);
		var file;
		while ((file = Global.readdir(dir)) != false) {
			if (file != '.' && file != '..') {
				list.push(file);
			}
		}
		Global.closedir(dir);
        return list;
	}
}
