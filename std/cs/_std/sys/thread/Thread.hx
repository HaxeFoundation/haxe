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

import cs.system.threading.Thread as NativeThread;
import cs.system.WeakReference;
import cs.Lib;

abstract Thread(HaxeThread) {
	public var events(get,never):EventLoop;

	inline function new(thread:HaxeThread) {
		this = thread;
	}

	public static function create(job:Void->Void):Thread {
		var hx:Null<HaxeThread> = null;
		var native = new NativeThread(job);
		native.IsBackground = true;
		hx = HaxeThread.allocate(native, false);
		native.Start();

		return new Thread(hx);
	}

	public static inline function runWithEventLoop(job:()->Void):Void {
		HaxeThread.runWithEventLoop(job);
	}

	public static inline function createWithEventLoop(job:()->Void):Thread {
		var hx:Null<HaxeThread> = null;
		var native = new NativeThread(() -> {
			job();
			if(hx == null) {
				HaxeThread.get(NativeThread.CurrentThread).events.loop();
			} else {
				hx.events.loop();
			}
		});
		native.IsBackground = true;
		hx = HaxeThread.allocate(native, true);
		native.Start();

		return new Thread(hx);
	}

	public static inline function current():Thread {
		return new Thread(HaxeThread.get(NativeThread.CurrentThread));
	}

	public static function readMessage(block:Bool):Dynamic {
		return current().readMessageImpl(block);
	}

	public inline function sendMessage(msg:Dynamic):Void {
		this.sendMessage(msg);
	}

	inline function readMessageImpl(block:Bool):Dynamic {
		return this.readMessage(block);
	}

	function get_events():EventLoop {
		if(this.events == null)
			throw new NoEventLoopException();
		return this.events;
	}

	@:keep
	static function initEventLoop():Void {
		@:privateAccess HaxeThread.get(NativeThread.CurrentThread).events = new EventLoop();
	}

	@:keep
	static function processEvents():Void {
		HaxeThread.get(NativeThread.CurrentThread).events.loop();
	}
}

private class HaxeThread {
	static final mainNativeThread = NativeThread.CurrentThread;
	static final mainHaxeThread = new HaxeThread(NativeThread.CurrentThread);
	static final threads = new Map<Int, WeakReference>();
	static final threadsMutex = new cs.system.threading.Mutex();
	static var allocateCount = 0;

	public final native:NativeThread;
	public var events(default,null):Null<EventLoop>;

	final messages = new Deque<Dynamic>();

	public static function get(native:NativeThread):HaxeThread {
		if(native == mainNativeThread) {
			return mainHaxeThread;
		}
		var native = NativeThread.CurrentThread;
		var key = native.ManagedThreadId;
		threadsMutex.WaitOne();
		var ref = threads.get(key);
		threadsMutex.ReleaseMutex();
		if (ref == null || !ref.IsAlive) {
			return allocate(native, false);
		}
		return ref.Target;
	}

	public static function allocate(native:NativeThread, withEventLoop:Bool):HaxeThread {
		threadsMutex.WaitOne();
		allocateCount++;
		inline function cleanup() {
			if (allocateCount % 100 == 0) {
				for (key => ref in threads) {
					if (!ref.IsAlive) {
						threads.remove(key);
					}
				}
			}
		}
		var hx = new HaxeThread(native);
		if(withEventLoop)
			hx.events = new EventLoop();
		var ref = new WeakReference(hx);
		cleanup();
		threads.set(native.ManagedThreadId, ref);
		threadsMutex.ReleaseMutex();
		return hx;
	}

	public static function runWithEventLoop(job:()->Void):Void {
		var thread = get(NativeThread.CurrentThread);
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

	function new(native:NativeThread) {
		this.native = native;
	}

	public inline function readMessage(block:Bool):Dynamic {
		return messages.pop(block);
	}

	public function sendMessage(msg:Dynamic):Void {
		messages.add(msg);
	}
}
