/*
 * Copyright (c) 2005, The haXe Project Contributors
 * All rights reserved.
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 *   - Redistributions of source code must retain the above copyright
 *     notice, this list of conditions and the following disclaimer.
 *   - Redistributions in binary form must reproduce the above copyright
 *     notice, this list of conditions and the following disclaimer in the
 *     documentation and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE HAXE PROJECT CONTRIBUTORS "AS IS" AND ANY
 * EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE HAXE PROJECT CONTRIBUTORS BE LIABLE FOR
 * ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
 * SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
 * CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH
 * DAMAGE.
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
		if (sys_create_dir( path, 493 )==null)
         throw "Could not create directory:" + path;
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
