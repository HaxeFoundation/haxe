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

import python.lib.threading.Thread as NativeThread;
import python.lib.Threading;

import haxe.ds.ObjectMap;

private typedef ThreadImpl = HxThread;

abstract Thread(ThreadImpl) from ThreadImpl {
	public var events(get,never):EventLoop;

	public static inline function current():Thread {
		return HxThread.current();
	}

	public static inline function create(job:Void->Void):Thread {
		return HxThread.create(job, false);
	}

	public static inline function runWithEventLoop(job:()->Void):Void {
		HxThread.runWithEventLoop(job);
	}

	public static inline function createWithEventLoop(job:()->Void):Thread {
		return HxThread.create(job, true);
	}

	public static inline function readMessage(block:Bool):Dynamic {
		return HxThread.readMessage(block);
	}

	public inline function sendMessage(msg:Dynamic):Void {
		this.sendMessage(msg);
	}

	function get_events():EventLoop {
		if(this.events == null)
			throw new NoEventLoopException();
		return this.events;
	}

	@:keep
	static public function processEvents() {
		HxThread.current().events.loop();
	}
}

private class HxThread {
	public var events(default,null):Null<EventLoop>;

	final nativeThread:NativeThread;
	final messages = new Deque<Dynamic>();

	static var threads:ObjectMap<NativeThread, HxThread>;
	static var threadsMutex:Mutex;
	static var mainThread:HxThread;

	static function __init__() {
		threads = new ObjectMap();
		threadsMutex = new Mutex();
		mainThread = new HxThread(Threading.current_thread());
		mainThread.events = new EventLoop();
	}

	private function new(t:NativeThread) {
		nativeThread = t;
	}

	public function sendMessage(msg:Dynamic):Void {
		messages.add(msg);
	}

	public static function current():HxThread {
		threadsMutex.acquire();
		var ct = Threading.current_thread();
		if (ct == Threading.main_thread()) {
			threadsMutex.release();
			return mainThread;
		}
		// If the current thread was not created via the haxe API, it can still be wrapped
		if (!threads.exists(ct)) {
			threads.set(ct, new HxThread(ct));
		}
		var t = threads.get(ct);
		threadsMutex.release();
		return t;
	}

	public static function create(callb:Void->Void, withEventLoop:Bool):HxThread {
		var nt:NativeThread = null;
		var t:HxThread = null;
		// Wrap the callback so it will clear the thread reference once the thread is finished
		var wrappedCallB = () -> {
			try {
				callb();
				if(withEventLoop)
					t.events.loop();
			} catch(e) {
				dropThread(nt);
				throw e;
			}
			dropThread(nt);
		}
		nt = new NativeThread({target:wrappedCallB});
		t = new HxThread(nt);
		if(withEventLoop)
			t.events = new EventLoop();
		threadsMutex.acquire();
		threads.set(nt, t);
		threadsMutex.release();
		nt.start();
		return t;
	}

	public static function runWithEventLoop(job:()->Void):Void {
		var thread = current();
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

	static inline function dropThread(nt:NativeThread) {
		threadsMutex.acquire();
		threads.remove(nt);
		threadsMutex.release();
	}

	public static function readMessage(block:Bool):Dynamic {
		return current().messages.pop(block);
	}
}
