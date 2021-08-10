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

/**
	Flags that can be passed to `hl.uv.FsEvent.start()` to control its behavior.

	@see http://docs.libuv.org/en/v1.x/fs_event.html#c.uv_fs_event_flags
**/
enum abstract FsEventFlag(Int) {
	var WATCH_ENTRY = 1;
	var STAT = 2;
	var RECURSIVE = 3;
}

/**
	Event types that `hl.uv.FsEvent` handles monitor.
**/
enum abstract FsEventType(Int) {
	var RENAME = 1;
	var CHANGE = 2;
}

/**
	FS Event handles allow the user to monitor a given path for changes

	@see http://docs.libuv.org/en/v1.x/fs_event.html
**/
@:forward
abstract FsEvent(Handle) to Handle {
	/**
		Initialize handle.
	**/
	@:hlNative("uv", "fs_event_init_wrap")
	static public function init(loop:Loop):FsEvent
		return null;

	/**
		Open file.
	**/
	public function start(path:String, flags:Null<Array<FsEventFlag>>, callback:(e:UVError, path:Null<String>, events:Null<Array<FsEventType>>)->Void):Void {
		startWrap(path, flags, (e, path, events) -> {
			var path = path == null ? null : @:privateAccess String.fromUTF8(path);
			var events = events == null ? null : [for(i in events) i];
			callback(e, path, events);
		});
	}
	@:hlNative("uv", "fs_event_start_wrap")
	function startWrap(path:String, flags:Null<Array<FsEventFlag>>, callback:(e:UVError, path:Null<Bytes>, events:Null<NativeArray<FsEventType>>)->Void):Void {}

	/**
		Stop the handle, the callback will no longer be called.
	**/
	@:hlNative("uv", "fs_event_stop_wrap")
	public function stop():Void {}
}