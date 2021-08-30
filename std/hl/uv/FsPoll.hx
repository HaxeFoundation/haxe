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

/**
	FS Poll handles allow the user to monitor a given path for changes.

	Unlike `hl.uv.FsEvent`, `FsPoll` handles use `stat` to detect when a file has
	changed so they can work on file systems where fs event handles can’t.

	@see http://docs.libuv.org/en/v1.x/fs_poll.html
**/
class FsPoll extends Handle<UvFsPollTStar> {
	var onChange:(status:Int, prev:UvStatTStar, curr:UvStatTStar)->Void;

	/**
		Initialize handle.
	**/
	static public function init(loop:Loop):FsPoll {
		loop.checkLoop();
		var poll = new FsPoll(UV.alloc_fs_poll());
		var result = loop.fs_poll_init(poll.h);
		if(result < 0) {
			poll.freeHandle();
			result.throwErr();
		}
		return poll;
	}

	/**
		Check the file at `path` for changes every `interval` milliseconds.
	**/
	public function start(path:String, interval:Int, callback:(e:UVError, previous:Null<FileStat>, current:Null<FileStat>)->Void):Void {
		handle(h -> {
			h.fs_poll_start_with_cb(path.toUTF8(), interval).resolve();
			onChange = (status, prev, curr) -> switch status.translate_uv_error() {
				case UV_NOERR: callback(UV_NOERR, File.uvStatToHl(prev), File.uvStatToHl(curr));
				case e: callback(e, null, null);
			}
		});
	}

	/**
		Stop the handle, the callback will no longer be called.
	**/
	public function stop():Void {
		handle(h -> h.fs_poll_stop().resolve());
	}

	/**
		Get the path being monitored by the handle.
	**/
	public function getPath():String {
		return handleReturn(h -> UV.getName((buf, size) -> h.fs_poll_getpath(buf,size)));
	}
}