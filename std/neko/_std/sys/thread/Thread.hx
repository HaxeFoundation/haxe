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

	public static inline function create(callb:Void->Void):Thread {
		return HaxeThread.create(callb);
	}

	public static inline function readMessage(block:Bool):Dynamic {
		return HaxeThread.readMessage(block);
	}

	inline function get_events():EventLoop {
		return this.events;
	}

	@:keep
	static function processEvents() {
		HaxeThread.current().events.loop();
	}

	/**
			Starts an OS message loop after [osInitialize] has been done.
			In that state, the UI handled by this thread will be updated and
			[sync] calls can be performed. The loop returns when [exitLoop] is
			called for this thread.
		**
		public static function osLoop() {
			if( os_loop == null ) throw "Please call osInitialize() first";
			os_loop();
		}

		/**
			The function [f] will be called by this thread if it's in [osLoop].
			[sync] returns immediately. See [osInitialize] remarks.
		**
		public function sync( f : Void -> Void ) {
			os_sync(handle,f);
		}

		/**
			The function [f] will be called by this thread and the calling thread
			will wait until the result is available then return its value.
		**
		public function syncResult<T>( f : Void -> T ) : T {
			if( this == current() )
				return f();
			var v = new neko.vm.Lock();
			var r = null;
			sync(function() {
				r = f();
				v.release();
			});
			v.wait();
			return r;
		}

		/**
			Exit from [osLoop].
		**
		public function exitLoop() {
			os_loop_stop(handle);
		}

		/**
			If you want to use the [osLoop], [sync] and [syncResult] methods, you
			need to call [osInitialize] before creating any thread or calling [current].
			This will load [os.ndll] library and initialize UI methods for each thread.
		**
		public static function osInitialize() {
			os_loop = neko.Lib.load("os","os_loop",0);
			os_loop_stop = neko.Lib.load("os","os_loop_stop",1);
			os_sync = neko.Lib.load("os","os_sync",2);
		}

		static var os_loop = null;
		static var os_loop_stop = null;
		static var os_sync = null;
	 */
}

@:callable
@:coreType
private abstract ThreadHandle {}

private class HaxeThread {
	static var thread_create:(callb:(_:Dynamic)->Void, _:Dynamic)->ThreadHandle;
	static var thread_current:()->ThreadHandle;
	static var thread_send:(handle:ThreadHandle, msg:Dynamic)->Void;
	static var thread_read_message:(block:Bool)->Dynamic;

	static var mainThreadHandle:ThreadHandle;
	static var mainThread:HaxeThread;

	static function __init__() {
		thread_create = neko.Lib.load("std", "thread_create", 2);
		thread_current = neko.Lib.load("std", "thread_current", 0);
		thread_send = neko.Lib.load("std", "thread_send", 2);
		thread_read_message = neko.Lib.load("std", "thread_read_message", 1);
		mainThreadHandle = thread_current();
		mainThread = new HaxeThread(mainThreadHandle);
	}

	static final threads = new Array<{thread:HaxeThread, handle:ThreadHandle}>();
	static final threadsMutex = new Mutex();

	public final events = new EventLoop();
	public var handle:ThreadHandle;

	static public function current():HaxeThread {
		var handle = thread_current();
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

	public static function create(callb:()->Void):Thread {
		var item = {handle:null, thread:new HaxeThread(null)};
		threadsMutex.acquire();
		threads.push(item);
		threadsMutex.release();
		item.handle = thread_create(_ -> {
			if(item.thread.handle == null) {
				item.handle = thread_current();
				item.thread.handle = item.handle;
			}
			try {
				callb();
				item.thread.events.loop();
			} catch(e) {
				dropThread(item);
				throw e;
			}
			dropThread(item);
		}, null);
		item.thread.handle = item.handle;
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

	public static inline function readMessage(block:Bool):Dynamic {
		return thread_read_message(block);
	}

	public function new(handle:ThreadHandle) {
		this.handle = handle;
	}

	public function sendMessage(msg:Dynamic) {
		thread_send(handle, msg);
	}
}