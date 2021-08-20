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

import hl.uv.File;

enum abstract DirEntryType(Int) {
	var UV_DIRENT_UNKNOWN = 0;
	var UV_DIRENT_FILE;
	var UV_DIRENT_DIR;
	var UV_DIRENT_LINK;
	var UV_DIRENT_FIFO;
	var UV_DIRENT_SOCKET;
	var UV_DIRENT_CHAR;
	var UV_DIRENT_BLOCK;
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
	var dir(get,never):Dir;
	inline function get_dir():Dir return cast this;

	/**
		Opens `path` as a directory stream
	**/
	static public function open(loop:Loop, path:String, callback:(e:UVError, dir:Null<Dir>)->Void):Void {
		var req = UV.alloc_fs();
		req.setCallback(() -> {
			var result = req.getResult();
			var dir = switch req.fs_get_ptr() {
				case null: null;
				case ptr: ptr.pointer_to_dir();
			}
			req.free();
			callback(result.translate_uv_error(), dir);
		});
		loop.fs_opendir_with_cb(req, path.toUTF8(), true).resolve();
	}

	/**
		Closes the directory stream.
	**/
	public function close(loop:Loop, callback:(e:UVError)->Void):Void {
		var req = UV.alloc_fs();
		req.setCallback(() -> {
			var result = req.getResult();
			req.free();
			callback(result.translate_uv_error());
		});
		loop.fs_closedir_with_cb(req, dir, true).resolve();
	}

	/**
		Iterates over the directory stream.
	**/
	public function read(loop:Loop, numberOfEntries:Int, callback:(e:UVError, entries:Null<Array<DirEntry>>)->Void):Void {
		var req = UV.alloc_fs();
		req.setCallback(() -> {
			var result = req.getResult();
			var e = result.translate_uv_error();
			var entries = switch e {
				case UV_NOERR:
					[for(i in 0...result) {
						var dirent = dir.dir_dirent(i);
						{
							name: dirent.dirent_name().fromUTF8(),
							type: dirent.dirent_type()
						}
					}];
				case _:
					null;
			}
			switch dir.dir_dirent(0) {
				case null:
				case dirent: dirent.dirent_to_pointer().free();
			}
			req.free();
			callback(e, entries);
		});
		dir.dir_init(numberOfEntries);
		loop.fs_readdir_with_cb(req, dir, true).resolve();
	}
}
