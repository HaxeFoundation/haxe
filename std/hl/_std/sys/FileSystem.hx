/*
 * Copyright (C)2005-2016 Haxe Foundation
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

@:coreApi
@:access(String)
class FileSystem {

	public static function exists( path : String ) : Bool {
		return sys_exists(makeCompatiblePath(path).bytes);
	}

	public static function rename( path : String, newPath : String ) : Void {
		if( !sys_rename(path.bytes, newPath.bytes) ) throw new Sys.SysError("Failed to rename " + path + " to " + newPath);
	}

	public static function stat( path : String ) : FileStat {
		var values = sys_stat(makeCompatiblePath(path).bytes);
		if( values == null ) throw new Sys.SysError("Failed to stat " + path);
		return {
			gid : values[0],
			uid : values[1],
			atime : @:privateAccess Date.fromInt(values[2]),
			mtime : @:privateAccess Date.fromInt(values[3]),
			ctime : @:privateAccess Date.fromInt(values[4]),
			size : values[5],
			dev : values[6],
			ino : values[7],
			nlink : values[8],
			rdev : values[9],
			mode : values[10],
		};
	}

	public static function fullPath( relPath : String ) : String {
		return String.fromUCS2(file_full_path(relPath.bytes));
	}

	public static function absolutePath ( relPath : String ) : String {
		if (haxe.io.Path.isAbsolute(relPath)) return relPath;
		return haxe.io.Path.join([Sys.getCwd(), relPath]);
	}

	public static function isDirectory( path : String ) : Bool {
		return sys_is_dir(makeCompatiblePath(path).bytes);
	}

	public static function createDirectory( path : String ) : Void {
		var path = haxe.io.Path.addTrailingSlash(path);
		var _p = null;
		var parts = [];
		while (path != (_p = haxe.io.Path.directory(path))) {
			parts.unshift(path);
			path = _p;
		}
		for (part in parts) {
			if (part.charCodeAt(part.length - 1) != ":".code && !exists(part))
				if( !sys_create_dir(part.bytes, 493) )
					throw new Sys.SysError("Failed to create directory " + part);
		}
	}

	public static function deleteFile( path : String ) : Void {
		if( !file_delete(path.bytes) ) throw new Sys.SysError("Can't delete file " + path);
	}

	public static function deleteDirectory( path : String ) : Void {
		if( !sys_remove_dir(path.bytes) ) throw new Sys.SysError("Can't delete directory " + path);
	}

	public static function readDirectory( path : String ) : Array<String> {
		var content = sys_read_dir(path.bytes);
		if( content == null )
			throw new Sys.SysError("Failed to read directory " + path);
		return [for( c in content ) String.fromUCS2(c)];
	}

	private static inline function makeCompatiblePath(path:String):String {
		return if (path.charCodeAt(1) == ":".code && path.length <= 3) {
			haxe.io.Path.addTrailingSlash(path);
		} else {
			haxe.io.Path.removeTrailingSlashes(path);
		}
	}

	@:hlNative("std", "sys_read_dir") static function sys_read_dir( path : hl.types.Bytes ) : hl.types.NativeArray<hl.types.Bytes> { return null; }
	@:hlNative("std", "sys_create_dir") static function sys_create_dir( path : hl.types.Bytes, rights : Int ) : Bool { return false; }
	@:hlNative("std", "sys_is_dir") static function sys_is_dir( path : hl.types.Bytes ) : Bool { return false; }
	@:hlNative("std", "sys_stat") static function sys_stat( path : hl.types.Bytes ) : hl.types.NativeArray<Int> { return null; }
	@:hlNative("std", "sys_rename") static function sys_rename( path : hl.types.Bytes, to : hl.types.Bytes ) : Bool { return true; }
	@:hlNative("std", "file_delete") static function file_delete( path : hl.types.Bytes ) : Bool { return true; };
	@:hlNative("std", "file_full_path") static function file_full_path( path : hl.types.Bytes ) : hl.types.Bytes { return null; }
	@:hlNative("std", "sys_remove_dir") static function sys_remove_dir( path : hl.types.Bytes ) : Bool { return true; }
	@:hlNative("std", "sys_exists") static function sys_exists( path : hl.types.Bytes ) : Bool { return true; }

}
