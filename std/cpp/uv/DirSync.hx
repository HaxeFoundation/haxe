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

package cpp.uv;

import cpp.uv.File;
import cpp.uv.Dir;

using cpp.uv.UV;

/**
	Synchronous handling of directory streams.

	@see http://docs.libuv.org/en/v1.x/fs.html#c.uv_fs_opendir
**/
@:headerCode('#include "uv.h"')
abstract DirSync(Dir) from Dir to Dir {

	/**
		Opens `path` as a directory stream
	**/
	static public function open(path:String):Dir {
		var req = new FsRequest();
		UV.fs_opendir(null, req.uvFs, path, null).resolve();
		switch req.getIntResult().explain() {
			case UV_NOERR:
				var dir = new Dir();
				dir.uvDir = cast UV.fs_get_ptr(req.uvFs);
				return dir;
			case e:
				throw new UVException(e);
		}
	}

	/**
		Closes the directory stream.
	**/
	public function close():Void {
		var req = new FsRequest();
		UV.fs_closedir(null, req.uvFs, this.uvDir, null).resolve();
		req.getIntResult().resolve();
	}

	/**
		Iterates over the directory stream.
	**/
	public function read(numberOfEntries:Int):Array<DirEntry> {
		var req = new FsRequest();
		var ptr = Pointer.fromRaw(this.uvDir);
		if(ptr.value.dirents != null)
			Stdlib.free(Pointer.fromRaw(ptr.value.dirents));
		ptr.value.nentries = numberOfEntries;
		ptr.value.dirents = untyped __cpp__('(uv_dirent_t *)malloc(sizeof(uv_dirent_t) * {0})', numberOfEntries);
		var result = UV.fs_readdir(null, req.uvFs, this.uvDir, null);
		inline function cleanup() {
			UV.fs_req_cleanup(req.uvFs);
			req.uv = null;
		}
		var result = req.getIntResult();
		switch result.explain() {
			case UV_NOERR:
				var ptr = Pointer.fromRaw(Pointer.fromRaw(this.uvDir).value.dirents);
				var entries:Array<DirEntry> = [for(i in 0...result) {
					var dirent = ptr.at(i);
					{
						name: dirent.name.toString(),
						type: new DirEntryType(dirent.type)
					}
				}];
				cleanup();
				return entries;
			case e:
				cleanup();
				throw new UVException(e);
		}
	}
}