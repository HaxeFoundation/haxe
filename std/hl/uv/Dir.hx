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

	@:allow(hl.uv)
	inline function new(v:Int)
		this = v;
}

typedef DirEntry = {
	var name:String;
	var type:DirEntryType;
}

/**
	Directory stream.

	@see http://docs.libuv.org/en/v1.x/fs.html#c.uv_fs_opendir
**/
@:allow(hl.uv)
class Dir {
	/** Synchronous versions of `hl.uv.Dir` methods */
	public var sync(get,never):DirSync;
	inline function get_sync():DirSync return this;

	var _d:UvDirTStar;

	function new(handle:UvDirTStar) {
		_d = handle;
	}

	extern inline function dir(action:(d:UvDirTStar)->Void) {
		switch _d {
			case null: throw new UVException(UV_EINVAL);
			case d: action(d);
		}
	}

	extern inline function dirReturn<R>(action:(d:UvDirTStar)->R):R {
		return switch _d {
			case null: throw new UVException(UV_EINVAL);
			case d: action(d);
		}
	}

	/**
		Opens `path` as a directory stream
	**/
	static public function open(loop:Loop, path:String, callback:(e:UVError, dir:Null<Dir>)->Void):FsRequest {
		loop.checkLoop();
		var req = File.createReq();
		var result = loop.fs_opendir_with_cb(req.r, path.toUTF8(), true);
		if(result < 0) {
			req.freeReq();
			result.throwErr();
		}
		req.callback = () -> {
			var result = req.getIntResult();
			var dir = switch req.r.fs_get_ptr() {
				case null: null;
				case ptr: ptr.pointer_to_dir();
			}
			req.freeReq();
			callback(result.translate_uv_error(), new Dir(dir));
		}
		return req;
	}

	/**
		Closes the directory stream.
	**/
	public function close(loop:Loop, callback:(e:UVError)->Void):FsRequest {
		loop.checkLoop();
		return dirReturn(d -> {
			var req = File.createReq();
			var result = loop.fs_closedir_with_cb(req.r, d, true);
			if(result < 0) {
				req.freeReq();
				result.throwErr();
			}
			req.callback = () -> {
				var result = req.getIntResult();
				req.freeReq();
				callback(result.translate_uv_error());
			};
			req;
		});
	}

	/**
		Iterates over the directory stream.
	**/
	public function read(loop:Loop, numberOfEntries:Int, callback:(e:UVError, entries:Null<Array<DirEntry>>)->Void):FsRequest {
		loop.checkLoop();
		return dirReturn(d -> {
			var req = File.createReq();
			d.dir_init(numberOfEntries);
			var result = loop.fs_readdir_with_cb(req.r, d, true);
			if(result < 0) {
				req.freeReq();
				result.throwErr();
			}
			req.callback = () -> {
				var result = req.getIntResult();
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
				callback(e, entries);
			}
			req;
		});
	}
}
