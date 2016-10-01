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

import lua.lib.lfs.Lfs;
import lua.Io;
import lua.Os;
import lua.Lib;
import lua.Table;
import haxe.io.Path;

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
		return  Os.rename(path, newPath);
	}

	public inline static function stat( path : String ) : FileStat {
		// the lua lfs attributes command uses a string for "mode".
		// we just need to patch it.

		var attr : sys.FileStat = cast Lfs.attributes(path);
		var lfs_mode : String = cast(attr.mode, String);
		var mode = switch(lfs_mode){
			case "file"         : 0x0100000;
			case "directory"    : 0x0040000;
			case "link"         : 0x0120000;
			case "socket"       : 0x0140000;
			case "named pipe"   : 0x0010000;
			case "char device"  : 0x0020000;
			case "block device" : 0x0060000;
			default             : 0x0000000;
		}
		attr.mode = mode;
		return attr;
	}

	public inline static function fullPath( relPath : String ) : String {
		return Path.normalize(absolutePath(relPath));
	}

	public inline static function absolutePath( relPath : String ) : String {
		if (relPath == null) return null;
		var pwd = Lfs.currentdir() ;
		if (pwd == null) return relPath;
		return Path.join([pwd, relPath]);
	}

	public inline static function deleteFile( path : String ) : Void {
		lua.Os.remove(path);
	}

	public inline static function readDirectory( path : String ) : Array<String> {
		var parts : Table<Dynamic, Dynamic> = lua.TableTools.pack(Lfs.dir(path));
		var itr = function(){
			var res = parts[1](parts[2]);
			while(res == "." || res == ".."){
				res = parts[1](parts[2]);
			}
			return res;
		}
		return lua.Lib.fillArray(itr);
	}

	public inline static function isDirectory( path : String ) : Bool {
		return  Lfs.attributes(path, "mode") ==  "directory";
	}

	public inline static function deleteDirectory( path : String ) : Void {
		Lfs.rmdir(path);
	}

	public inline static function createDirectory( path : String ) : Void {
	   Lfs.mkdir(path);
	}
}
