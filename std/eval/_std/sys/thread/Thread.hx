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

package sys.thread;

import eval.vm.NativeThread;

private typedef ThreadImpl = NativeThread;

abstract Thread(ThreadImpl) from ThreadImpl {
	public var events(get,never):EventLoop;

	static function __init__() {
		NativeThread.self().events = new EventLoop();
	}

	inline function new(h:NativeThread):Void {
		this = h;
	}

	public inline function sendMessage(msg:Dynamic):Void {
		this.sendMessage(msg);
	}

	public static inline function current():Thread {
		return new Thread(NativeThread.self());
	}

	public static inline function create(job:()->Void):Thread {
		return new Thread(new NativeThread(job));
	}

	public static function runWithEventLoop(job:()->Void):Void {
		var thread = NativeThread.self();
		if(thread.events == null) {
			thread.events = new EventLoop();
			try {
				job();
				thread.events.loop();
				thread.events = null;
			} catch(e) {
				thread.events = null;
				throw e;
			}
		} else {
			job();
		}
	}

	public static inline function createWithEventLoop(job:()->Void):Thread {
		return new Thread(new NativeThread(() -> {
			var thread = NativeThread.self();
			thread.events = new EventLoop();
			job();
			thread.events.loop();
		}));
	}

	public static inline function readMessage(block:Bool):Dynamic {
		return NativeThread.readMessage(block);
	}

	public static inline function yield():Void {
		NativeThread.yield();
	}

	@:op(A == B)
	public inline function equals(other:Thread):Bool {
		return getHandle().id() == other.getHandle().id();
	}

	inline function getHandle():NativeThread {
		return this;
	}

	function get_events():EventLoop {
		if(this.events == null)
			throw new NoEventLoopException();
		return this.events;
	}

	@:keep
	static function processEvents():Void {
		NativeThread.self().events.loop();
	}
}
