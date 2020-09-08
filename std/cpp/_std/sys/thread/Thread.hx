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

abstract Thread(HaxeThread) from HaxeThread to HaxeThread {
	public var events(get,never):EventLoop;

	public inline function sendMessage(msg:Dynamic):Void {
		this.sendMessage(msg);
	}

	public static inline function current():Thread {
		return HaxeThread.current();
	}

	public static inline function create(job:()->Void):Thread {
		return HaxeThread.create(job, false);
	}

	public static inline function runWithEventLoop(job:()->Void):Void {
		HaxeThread.runWithEventLoop(job);
	}

	public static inline function createWithEventLoop(job:()->Void):Thread {
		return HaxeThread.create(job, true);
	}

	public static function readMessage(block:Bool):Dynamic {
		return HaxeThread.readMessage(block);
	}

	inline function get_events():EventLoop {
		if(this.events == null)
			throw new NoEventLoopException();
		return this.events;
	}

	@:keep
	static function initEventLoop() {
		@:privateAccess HaxeThread.current().events = new EventLoop();
	}

	@:keep
	static public function processEvents() {
		HaxeThread.current().events.loop();
	}
}

@:callable
@:coreType
private abstract ThreadHandle {}

private class HaxeThread {
	static final threads = new Array<{thread:HaxeThread, handle:ThreadHandle}>();
	static final threadsMutex = new Mutex();
	static var mainThreadHandle:ThreadHandle = currentHandle();
	static var mainThread:HaxeThread = new HaxeThread(currentHandle());

	public var events(default,null):Null<EventLoop>;
	public var handle:ThreadHandle;
	final messages = new Deque<Dynamic>();

	static public function current():HaxeThread {
		var handle = currentHandle();
		if(handle == mainThreadHandle) {
			return mainThread;
		}
		threadsMutex.acquire();
		var thread = null;
		for(item in threads) {
			if(item.handle == handle) {
				thread = item.thread;
				break;
			}
		}
		if(thread == null) {
			thread = new HaxeThread(handle);
			threads.push({thread:thread, handle:handle});
		}
		threadsMutex.release();
		return thread;
	}

	public static function create(job:()->Void, withEventLoop:Bool):Thread {
		var item = {handle:null, thread:new HaxeThread(null)};
		threadsMutex.acquire();
		var index = threads.push(item);
		threadsMutex.release();
		if(withEventLoop)
			item.thread.events = new EventLoop();
		item.handle = createHandle(() -> {
			if(item.thread.handle == null) {
				item.handle = currentHandle();
				item.thread.handle = item.handle;
			}
			try {
				job();
				if(withEventLoop)
					item.thread.events.loop();
			} catch(e) {
				dropThread(item, index);
				throw e;
			}
			dropThread(item, index);
		});
		item.thread.handle = item.handle;
		return item.thread;
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

	static function dropThread(item, probableIndex:Int) {
		threadsMutex.acquire();
		if(threads[probableIndex] == item) {
			threads.splice(probableIndex, 1);
		} else {
			for(i => item2 in threads) {
				if(item2 == item) {
					threads.splice(i, 1);
					break;
				}
			}
		}
		threadsMutex.release();
	}

	function new(h:ThreadHandle):Void {
		handle = h;
	}

	public inline function sendMessage(msg:Dynamic):Void {
		messages.add(msg);
	}

	static inline function currentHandle():ThreadHandle {
		return untyped __global__.__hxcpp_thread_current();
	}

	static inline function createHandle(callb:Void->Void):ThreadHandle {
		return untyped __global__.__hxcpp_thread_create(callb);
	}

	public static inline function readMessage(block:Bool):Dynamic {
		return current().messages.pop(block);
	}
}
