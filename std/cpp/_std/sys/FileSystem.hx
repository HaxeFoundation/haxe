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
package sys;

private enum FileKind {
	kdir;
	kfile;
	kother( k : String );
}

@:coreApi
class FileSystem {

	public static inline function exists( path : String ) : Bool {
		return sys_exists(path);
	}

	public static function rename( path : String, newpath : String ) : Void {
		if (sys_rename(path,newpath)==null)
         throw "Could not rename:" + path + " to " + newpath;
	}

	public static function stat( path : String ) : FileStat {
		var s : FileStat = sys_stat(path);
		if (s==null)
			return { gid:0, uid:0, atime:Date.fromTime(0), mtime:Date.fromTime(0), ctime:Date.fromTime(0), dev:0, ino:0, nlink:0, rdev:0, size:0, mode:0 };
		s.atime = Date.fromTime(1000.0*(untyped s.atime));
		s.mtime = Date.fromTime(1000.0*(untyped s.mtime));
		s.ctime = Date.fromTime(1000.0*(untyped s.ctime));
		return s;
	}

	public static function fullPath( relpath : String ) : String {
		return new String(file_full_path(relpath));
	}

	static function kind( path : String ) : FileKind {
		var k:String = sys_file_type(path);
		return switch(k) {
		case "file": kfile;
		case "dir": kdir;
		default: kother(k);
		}
	}

	public static function isDirectory( path : String ) : Bool {
		return kind(path) == kdir;
	}

	public static function createDirectory( path : String ) : Void {
		var path = haxe.io.Path.addTrailingSlash(path);
		var parts = [while ((path = haxe.io.Path.directory(path)) != "") path];
		parts.reverse();
		for (part in parts) {
			if (!exists(part) && sys_create_dir( part, 493 )==null)
				throw "Could not create directory:" + part;
		}
	}

	public static function deleteFile( path : String ) : Void {
		if (file_delete(path)==null)
         throw "Could not delete file:" + path;
	}

	public static function deleteDirectory( path : String ) : Void {
		if (sys_remove_dir(path)==null)
         throw "Could not delete directory:" + path;
	}

	public static function readDirectory( path : String ) : Array<String> {
		return sys_read_dir(path);
	}

	private static var sys_exists = cpp.Lib.load("std","sys_exists",1);
	private static var file_delete = cpp.Lib.load("std","file_delete",1);
	private static var sys_rename = cpp.Lib.load("std","sys_rename",2);
	private static var sys_stat = cpp.Lib.load("std","sys_stat",1);
	private static var sys_file_type = cpp.Lib.load("std","sys_file_type",1);
	private static var sys_create_dir = cpp.Lib.load("std","sys_create_dir",2);
	private static var sys_remove_dir = cpp.Lib.load("std","sys_remove_dir",1);
	private static var sys_read_dir = cpp.Lib.load("std","sys_read_dir",1);
	private static var file_full_path = cpp.Lib.load("std","file_full_path",1);

}
