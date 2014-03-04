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
		return untyped __call__("file_exists", path);
	}

	public static inline function rename( path : String, newPath : String ) : Void {
		untyped __call__("rename", path, newPath);
	}

	public static function stat( path : String ) : FileStat {
		untyped __php__("$fp = fopen($path, \"r\"); $fstat = fstat($fp); fclose($fp);");
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

	public static inline function fullPath( relPath : String ) : String {
		var p = untyped __call__("realpath", relPath);
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
		var path = haxe.io.Path.addTrailingSlash(path);
		var parts = [while ((path = haxe.io.Path.directory(path)) != "") path];
		parts.reverse();
		for (part in parts) {
			if (part.charCodeAt(part.length - 1) != ":".code && !exists(part))
				untyped __call__("@mkdir", part, 493); // php default is 0777, neko is 0755
		}
	}

	public static inline function deleteFile( path : String ) : Void {
		untyped __call__("@unlink", path);
	}

	public static inline function deleteDirectory( path : String ) : Void {
		untyped __call__("@rmdir", path);
	}

	public static function readDirectory( path : String ) : Array<String> {
		var l = untyped __call__("array");
		untyped __php__("$dh = opendir($path);
        while (($file = readdir($dh)) !== false) if(\".\" != $file && \"..\" != $file) $l[] = $file;
        closedir($dh);");
		return untyped __call__("new _hx_array", l);
	}
}
