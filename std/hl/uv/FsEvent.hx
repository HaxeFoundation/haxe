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
enum abstract FsEventFlag(Int) to Int {
	var WATCH_ENTRY = 1;
	var STAT = 2;
	var RECURSIVE = 4;
}

/**
	Event types that `hl.uv.FsEvent` handles monitor.
**/
enum abstract FsEventType(Int) to Int {
	var RENAME = 1;
	var CHANGE = 2;
}

abstract FsEvents(Int) {
	@:allow(hl.uv.FsEvent)
	inline function new(events:Int) {
		this = events;
	}

	public var rename(get,never):Bool;
	inline function get_rename():Bool
		return 0 != (this & RENAME);

	public var change(get,never):Bool;
	inline function get_change():Bool
		return 0 != (this & CHANGE);
}

/**
	FS Event handles allow the user to monitor a given path for changes

	@see http://docs.libuv.org/en/v1.x/fs_event.html
**/
class FsEvent extends Handle<UvFsEventTStar> {
	var callback:(status:Int, path:Bytes, evens:Int)->Void;

	/**
		Initialize handle.
	**/
	static public function init(loop:Loop):FsEvent {
		loop.checkLoop();
		var event = new FsEvent(UV.alloc_fs_event());
		var result = loop.fs_event_init(event.h);
		if(result < 0) {
			event.freeHandle();
			result.throwErr();
		}
		return event;
	}

	/**
		Start the handle with the given callback, which will watch the specified
		path for changes.
	**/
	public function start(path:String, flags:Null<Array<FsEventFlag>>, callback:(e:UVError, path:Null<String>, events:FsEvents)->Void):Void {
		handle(h -> {
			var cFlags = 0;
			if(flags != null)
				for(f in flags)
					cFlags |= f;
			h.fs_event_start_with_cb(path.toUTF8(), cFlags).resolve();
			this.callback = (status, path, events) -> {
				callback(status.translate_uv_error(), (path == null ? null : path.fromUTF8()), new FsEvents(events));
			}
		});
	}

	/**
		Stop the handle, the callback will no longer be called.
	**/
	public function stop():Void {
		handle(h -> h.fs_event_stop().resolve());
	}

	/**
		Get the path being monitored by the handle.
	**/
	public function getPath():String {
		return handleReturn(h -> UV.getName((buf, size) -> h.fs_event_getpath(buf,size)));
	}

}