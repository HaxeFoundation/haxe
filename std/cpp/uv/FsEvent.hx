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

using cpp.uv.UV;

enum abstract FsEventFlag(Int) to Int {
	var WATCH_ENTRY = 1;
	var STAT = 2;
	var RECURSIVE = 4;
}

enum abstract FsEventType(Int) to Int {
	var RENAME = 1;
	var CHANGE = 2;
}

abstract FsEvents(Int) {
	@:allow(cpp.uv.FsEvent)
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
	FS Event handles allow the user to monitor a given path for changes, for example,
	if the file was renamed or there was a generic change in it.

	@see http://docs.libuv.org/en/v1.x/fs_event.html
**/
@:headerCode('#include "uv.h"')
class FsEvent extends Handle {
	var uvFsEvent:RawPointer<UvFsEventT>;
	var onEvent:(e:UVError, path:Null<String>, events:FsEvents)->Void;

	function setupUvHandle() {
		uvFsEvent = UvFsEventT.create();
		uvHandle = cast uvFsEvent;
	}

	/**
		Initialize a new FsEvent handle.
	**/
	static public function init(loop:Loop):FsEvent {
		var event = new FsEvent();
		UV.fs_event_init(loop.uvLoop, event.uvFsEvent).resolve();
		return event;
	}

	/**
		Start the handle with the given callback, which will watch the specified
		path for changes.
	**/
	public function start(path:String, flags:Null<Array<FsEventFlag>>, callback:(e:UVError, path:Null<String>, events:FsEvents)->Void) {
		var iFlags = 0;
		if(flags != null)
			for(flag in flags)
				iFlags |= flag;
		UV.fs_event_start(uvFsEvent, Callable.fromStaticFunction(uvFsEventCb), path, iFlags).resolve();
		onEvent = callback;
	}

	static function uvFsEventCb(uvFsEvent:RawPointer<UvFsEventT>, filename:ConstCharStar, events:Int, status:Int) {
		var event:FsEvent = cast Handle.getHandle(cast uvFsEvent);
		event.onEvent(status.explain(), filename == null ? null : filename.toString(), new FsEvents(events));
	}

	/**
		Stop the handle, the callback will no longer be called.
	**/
	public function stop() {
		UV.fs_event_stop(uvFsEvent).resolve();
		onEvent = null;
	}

	/**
		Get the path being monitored by the handle.
	**/
	public function getPath():String {
		return UV.getName((buf, size) -> UV.fs_event_getpath(uvFsEvent, buf, size));
	}
}