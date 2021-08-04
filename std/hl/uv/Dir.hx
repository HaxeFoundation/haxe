/*
 * Copyright (C)2005-2019 Haxe Foundation
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

package hl.uv;

enum abstract DirEntryType(Int) {
	var DIRENT_UNKNOWN = 1;
	var DIRENT_FILE = 2;
	var DIRENT_DIR = 3;
	var DIRENT_LINK = 4;
	var DIRENT_FIFO = 5;
	var DIRENT_SOCKET = 6;
	var DIRENT_CHAR = 7;
	var DIRENT_BLOCK = 8;
}

typedef DirEntry = {
	var name:String;
	var type:DirEntryType;
}

/**
	Directory stream.

	@see http://docs.libuv.org/en/v1.x/fs.html#c.uv_fs_opendir
**/
abstract Dir(Abstract<"uv_dir">) {
	/**
		Opens `path` as a directory stream
	**/
	@:hlNative("uv", "fs_opendir_wrap")
	static public function open(loop:Loop, path:String, callback:(e:UVError, dir:Null<Dir>)->Void):Void {}

	/**
		Closes the directory stream.
	**/
	@:hlNative("uv", "fs_closedir_wrap")
	public function close(loop:Loop, callback:(e:UVError)->Void):Void {}

	/**
		Iterates over the directory stream.
	**/
	public function read(loop:Loop, numberOfEntries:Int, callback:(e:UVError, entries:Null<Array<DirEntry>>)->Void):Void {
		readWrap(loop, numberOfEntries, (e, entries) -> {
			if(entries == null) {
				callback(e, null);
			} else {
				callback(e, [for(i in 0...entries.length) {
					var entry:Dynamic = entries[i];
					entry.name = @:privateAccess String.fromUTF8(entry.name);
					entry;
				}]);
			}
		});
	}

	@:hlNative("uv", "fs_readdir_wrap")
	function readWrap(loop:Loop, numberOfEntries:Int, callback:(e:UVError, entries:NativeArray<Dynamic>)->Void):Void {}
}
