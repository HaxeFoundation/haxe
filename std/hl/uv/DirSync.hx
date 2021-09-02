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
import hl.uv.Dir;

/**
	Synchronous handling of directory streams.

	@see http://docs.libuv.org/en/v1.x/fs.html#c.uv_fs_opendir
**/
abstract DirSync(Dir) from Dir to Dir {
	/**
		Opens `path` as a directory stream
	**/
	static public function open(path:String):Dir {
		var req = File.createReq();
		var result = UV.fs_opendir_with_cb(null, req.r, path.toUTF8(), false);
		if(result >= 0)
			result = req.getIntResult();
		var dir = switch req.r.fs_get_ptr() {
			case null: null;
			case ptr: ptr.pointer_to_dir();
		}
		req.freeReq();
		result.resolve();
		return new Dir(dir);
	}

	/**
		Closes the directory stream.
	**/
	public function close():Void {
		this.dir(d -> {
			var req = File.createReq();
			var result = UV.fs_closedir_with_cb(null, req.r, d, false);
			if(result >= 0)
				result = req.getIntResult();
			req.freeReq();
			result.resolve();
		});
	}

	/**
		Iterates over the directory stream.
	**/
	public function read(numberOfEntries:Int):Array<DirEntry> {
		return this.dirReturn(d -> {
			var req = File.createReq();
			d.dir_init(numberOfEntries);
			var result = UV.fs_readdir_with_cb(null, req.r, d, false);
			if(result >= 0)
				result = req.getIntResult();
			var e = result.translate_uv_error();
			var entries = switch e {
				case UV_NOERR:
					[for(i in 0...result) {
						var dirent = d.dir_dirent(i);
						{
							name: dirent.dirent_name().fromUTF8(),
							type: new DirEntryType(dirent.dirent_type())
						}
					}];
				case _:
					null;
			}
			req.freeReq();
			entries;
		});
	}
}
