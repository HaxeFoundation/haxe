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

	public static function exists( path : String ) : Bool {
		return sys_exists(untyped path.__s);
	}

	public static function rename( path : String, newpath : String ) : Void {
		untyped sys_rename(path.__s,newpath.__s);
	}

	public static function stat( path : String ) : FileStat {
		var s : FileStat = sys_stat(untyped path.__s);
		s.atime = untyped Date.new1(s.atime);
		s.mtime = untyped Date.new1(s.mtime);
		s.ctime = untyped Date.new1(s.ctime);
		return s;
	}

	public static function fullPath( relpath : String ) : String {
		return new String(file_full_path(untyped relpath.__s));
	}

	static function kind( path : String ) : FileKind {
		var k = new String(sys_file_type(untyped path.__s));
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
			if (!exists(part))
				sys_create_dir( untyped part.__s, 493 );
		}
	}

	public static function deleteFile( path : String ) : Void {
		file_delete(untyped path.__s);
	}

	public static function deleteDirectory( path : String ) : Void {
		sys_remove_dir(untyped path.__s);
	}

	public static function readDirectory( path : String ) : Array<String> {
		var l : Array<Dynamic> = sys_read_dir(untyped path.__s);
		var a = new Array();
		while( l != null ) {
			a.push(new String(l[0]));
			l = l[1];
		}
		return a;
	}

	private static var sys_exists = neko.Lib.load("std","sys_exists",1);
	private static var file_delete = neko.Lib.load("std","file_delete",1);
	private static var sys_rename = neko.Lib.load("std","sys_rename",2);
	private static var sys_stat = neko.Lib.load("std","sys_stat",1);
	private static var sys_file_type = neko.Lib.load("std","sys_file_type",1);
	private static var sys_create_dir = neko.Lib.load("std","sys_create_dir",2);
	private static var sys_remove_dir = neko.Lib.load("std","sys_remove_dir",1);
	private static var sys_read_dir = neko.Lib.load("std","sys_read_dir",1);
	private static var file_full_path = neko.Lib.load("std","file_full_path",1);

}
