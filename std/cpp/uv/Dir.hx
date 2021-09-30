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

using cpp.uv.UV;

enum abstract DirEntryType(Int) {
	var DIRENT_UNKNOWN;
	var DIRENT_FILE;
	var DIRENT_DIR;
	var DIRENT_LINK;
	var DIRENT_FIFO;
	var DIRENT_SOCKET;
	var DIRENT_CHAR;
	var DIRENT_BLOCK;

	@:allow(cpp.uv)
	inline function new(uv:UvDirentTypeT)
		this = uv;
}

@:structInit
class DirEntry {
	var name:String;
	var type:DirEntryType;

	public function toString():String {
		return '{name:$name, type:$type}';
	}
}


/**
	Directory stream.

	@see http://docs.libuv.org/en/v1.x/fs.html#c.uv_fs_opendir
**/
@:allow(cpp.uv.DirSync)
@:headerCode('#include "uv.h"')
class Dir {
	/** Synchronous operations with this directory */
	public var sync(get,never):DirSync;
	inline function get_sync():DirSync return this;

	var uvDir:RawPointer<UvDirT>;

	function new() {
		cpp.vm.Gc.setFinalizer(this, Function.fromStaticFunction(finalizer));
	}

	static function finalizer(dir:Dir) {
		Stdlib.free(Pointer.fromRaw(dir.uvDir));
	}

	/**
		Opens `path` as a directory stream
	**/
	static public function open(loop:Loop, path:String, callback:(e:UVError, dir:Null<Dir>)->Void):FsRequest {
		var req = new FsRequest(loop);
		UV.fs_opendir(loop.uvLoop, req.uvFs, path, Callable.fromStaticFunction(File.uvFsCb)).resolve();
		req.referenceFromLoop();
		req.callback = () -> {
			switch req.getIntResult().explain() {
				case UV_NOERR:
					var dir = new Dir();
					dir.uvDir = cast UV.fs_get_ptr(req.uvFs);
					callback(UV_NOERR, dir);
				case e:
					callback(e, null);
			}
		}
		return req;
	}

	/**
		Closes the directory stream.
	**/
	public function close(loop:Loop, callback:(e:UVError)->Void):FsRequest {
		var req = new FsRequest(loop);
		UV.fs_closedir(loop.uvLoop, req.uvFs, uvDir, Callable.fromStaticFunction(File.uvFsCb)).resolve();
		req.referenceFromLoop();
		req.callback = () -> callback(req.getIntResult().explain());
		return req;
	}

	/**
		Iterates over the directory stream.
	**/
	public function read(loop:Loop, numberOfEntries:Int, callback:(e:UVError, entries:Null<Array<DirEntry>>)->Void):FsRequest {
		var req = new FsRequest(loop);
		var ptr = Pointer.fromRaw(uvDir);
		if(ptr.value.dirents != null)
			Stdlib.free(Pointer.fromRaw(ptr.value.dirents));
		ptr.value.nentries = numberOfEntries;
		ptr.value.dirents = untyped __cpp__('(uv_dirent_t *)malloc(sizeof(uv_dirent_t) * {0})', numberOfEntries);
		UV.fs_readdir(loop.uvLoop, req.uvFs, uvDir, Callable.fromStaticFunction(File.uvFsCb)).resolve();
		req.referenceFromLoop();
		req.callback = () -> {
			inline function cleanup() {
				UV.fs_req_cleanup(req.uvFs);
				req.uv = null;
			}
			var result = req.getIntResult();
			switch result.explain() {
				case UV_NOERR:
					var ptr = Pointer.fromRaw(Pointer.fromRaw(uvDir).value.dirents);
					var entries:Array<DirEntry> = [for(i in 0...result) {
						var dirent = ptr.at(i);
						{
							name: dirent.name.toString(),
							type: new DirEntryType(dirent.type)
						}
					}];
					cleanup();
					callback(UV_NOERR, entries);
				case e:
					cleanup();
					callback(e, null);
			}
		}
		return req;
	}
}