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

	public inline function sendMessage(msg:Dynamic) {
		this.sendMessage(msg);
	}

	public static inline function readMessage(block = true):Dynamic {
		return HaxeThread.current().readMessage(block);
	}

	public static inline function create(callb:Void->Void):Thread {
		return HaxeThread.create(callb);
	}

	public static function current():Thread {
		return HaxeThread.current();
	}

	inline function get_events():EventLoop {
		return this.events;
	}

	@:keep
	static public function processEvents() {
		HaxeThread.current().events.loop();
	}
}

private typedef ThreadHandle = hl.Abstract<"hl_thread">;

private class HaxeThread {
	static final mainThreadHandle:ThreadHandle = currentHandle();
	static final mainThread:HaxeThread = new HaxeThread();
	static final threads = new Array<{thread:HaxeThread, handle:ThreadHandle}>();
	static final threadsMutex = new Mutex();

	public final events = new EventLoop();
	final messages = new Deque();

	static var ids = 0;
	var id = ids++;

	@:hlNative("std", "thread_create")
	static function createHandle(callb:Void->Void):ThreadHandle {
		return null;
	}

	@:hlNative("std", "thread_current")
	static function currentHandle():ThreadHandle {
		return null;
	}

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
			thread = new HaxeThread();
			threads.push({thread:thread, handle:handle});
		}
		threadsMutex.release();
		return thread;
	}

	public static function create(callb:()->Void):Thread {
		var item = {handle:null, thread:new HaxeThread()};
		threadsMutex.acquire();
		threads.push(item);
		threadsMutex.release();
		item.handle = createHandle(() -> {
			if(item.handle == null) {
				item.handle = currentHandle();
			}
			try {
				callb();
				item.thread.events.loop();
			} catch(e) {
				dropThread(item);
				throw e;
			}
			dropThread(item);
		});
		return item.thread;
	}

	static function dropThread(deleteItem) {
		threadsMutex.acquire();
		for(i => item in threads) {
			if(item == deleteItem) {
				threads.splice(i, 1);
				break;
			}
		}
		threadsMutex.release();
	}

	public function readMessage(block:Bool):Dynamic {
		return messages.pop(block);
	}

	public function new() {}

	public function sendMessage(msg:Dynamic) {
		messages.add(msg);
	}
}
