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

/**
	FS Poll handles allow the user to monitor a given path for changes.

	Unlike `cpp.uv.FsEvent`, fs poll handles use stat to detect when a file has
	changed so they can work on file systems where fs event handles can’t.

	@see http://docs.libuv.org/en/v1.x/fs_poll.html
**/
@:headerCode('#include "uv.h"')
class FsPoll extends Handle {
	var onChange:(e:UVError, previous:Null<FileStat>, current:Null<FileStat>)->Void;
	var uvFsPoll(get,never):RawPointer<UvFsPollT>;

	inline function get_uvFsPoll():RawPointer<UvFsPollT>
		return cast uv;

	override function setupUvData() {
		uv = cast UvFsPollT.create();
		super.setupUvData();
	}

	/**
		Initialize a new FsPoll handle.
	**/
	static public function init(loop:Loop):FsPoll {
		var poll = new FsPoll(loop);
		UV.fs_poll_init(loop.uvLoop, poll.uvFsPoll).resolve();
		poll.referenceFromLoop();
		return poll;
	}

	/**
		Check the file at `path` for changes every `interval` milliseconds.
	**/
	public function start(path:String, interval:UInt32, callback:(e:UVError, previous:Null<FileStat>, current:Null<FileStat>)->Void) {
		UV.fs_poll_start(uvFsPoll, Callable.fromStaticFunction(uvFsPollCb), path, interval).resolve();
		onChange = callback;
	}

	static function uvFsPollCb(uvFsPoll:RawPointer<UvFsPollT>, status:Int, prev:RawConstPointer<UvStatT>, curr:RawConstPointer<UvStatT>) {
		var poll:FsPoll = cast Handle.get(cast uvFsPoll);
		switch status.explain() {
			case UV_NOERR:
				poll.onChange(UV_NOERR, File.uvStatToHx(prev), File.uvStatToHx(curr));
			case e:
				poll.onChange(e, null, null);
		}

	}

	/**
		Stop the handle, the callback will no longer be called.
	**/
	public function stop() {
		UV.fs_poll_stop(uvFsPoll).resolve();
		onChange = null;
	}

	/**
		Get the path being monitored by the handle.
	**/
	public function getPath():String {
		return UV.getName((buf, size) -> UV.fs_poll_getpath(uvFsPoll, buf, size));
	}
}