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

	public static inline function exists( path : String ) : Bool {
		return untyped __call__("file_exists", path);
	}

	public static inline function rename( path : String, newpath : String ) : Void {
		untyped __call__("rename", path, newpath);
	}

	public static function stat( path : String ) : FileStat {
		untyped __php__('$fp = fopen($path, "r");
		$fstat = fstat($fp);
		fclose($fp);');
		return untyped {
			gid   : __php__("$fstat['gid']"),
			uid   : __php__("$fstat['uid']"),
			atime : Date.fromTime(__php__("$fstat['atime']")*1000),
			mtime : Date.fromTime(__php__("$fstat['mtime']")*1000),
			ctime : Date.fromTime(__php__("$fstat['ctime']")*1000),
			dev   : __php__("$fstat['dev']"),
			ino   : __php__("$fstat['ino']"),
			nlink : __php__("$fstat['nlink']"),
			rdev  : __php__("$fstat['rdev']"),
			size  : __php__("$fstat['size']"),
			mode  : __php__("$fstat['mode']")
		};
	}

	public static inline function fullPath( relpath : String ) : String {
		var p = untyped __call__("realpath", relpath);
		if (untyped __physeq__(p, false))
			return null;
		else
			return p;
	}

	static function kind( path : String ) : FileKind {
		var k = untyped __call__("filetype", path);
		switch(k) {
			case "file": return kfile;
			case "dir": return kdir;
			default: return kother(k);
		}
	}

	public static inline function isDirectory( path : String ) : Bool {
		return untyped __call__("is_dir", path);
	}

	public static inline function createDirectory( path : String ) : Void {
		untyped __call__("@mkdir", path, 493); // php default is 0777, neko is 0755
	}

	public static inline function deleteFile( path : String ) : Void {
		untyped __call__("@unlink", path);
	}

	public static inline function deleteDirectory( path : String ) : Void {
		untyped __call__("@rmdir", path);
	}

	public static function readDirectory( path : String ) : Array<String> {
		var l = untyped __call__("array");
		untyped __php__('$dh = opendir($path);
        while (($file = readdir($dh)) !== false) if("." != $file && ".." != $file) $l[] = $file;
        closedir($dh);');
		return untyped __call__("new _hx_array", l);
	}
}
