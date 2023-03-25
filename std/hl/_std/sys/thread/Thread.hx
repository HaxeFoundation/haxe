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

private typedef ThreadImpl = HaxeThread;

abstract Thread(ThreadImpl) from ThreadImpl {
	public var events(get,never):EventLoop;

	public inline function sendMessage(msg:Dynamic) {
		this.sendMessage(msg);
	}

	public static inline function readMessage(block = true):Dynamic {
		return HaxeThread.current().readMessage(block);
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

	public static function current():Thread {
		return HaxeThread.current();
	}


	public function setName( name : String ) {
		#if (hl_ver >= version("1.13.0"))
		set_name(@:privateAccess this.handle, @:privateAccess name.toUtf8());
		#end
	}

	public function getName() : Null<String> {
		#if (hl_ver >= version("1.13.0"))
		var name = get_name(@:privateAccess this.handle);
		return name == null ? null : @:privateAccess String.fromUTF8(name);
		#else
		return null;
		#end
	}

	#if (hl_ver >= version("1.13.0"))
	@:hlNative("?std", "thread_set_name") static function set_name( t : ThreadHandle, name : hl.Bytes ) {}
	@:hlNative("?std", "thread_get_name") static function get_name( t : ThreadHandle ) : hl.Bytes { return null; }
	#end

	function get_events():EventLoop {
		if(this.events == null)
			throw new NoEventLoopException();
		return this.events;
	}

	@:keep
	static public function processEvents() {
		HaxeThread.current().events.loop();
	}
}

private typedef ThreadHandle = hl.Abstract<"hl_thread">;

private class HaxeThread {

	static var mainThread:HaxeThread;
	static var threads:Array<HaxeThread>;
	static var threadsMutex:Mutex;
	static var UID = 0;

	static function __init__() {
		threadsMutex = new Mutex();
		threads = [];
		mainThread = new HaxeThread(currentHandle());
		mainThread.events = new EventLoop();
	}

	var id = UID++;
	public var events(default,null):Null<EventLoop>;
	var handle : ThreadHandle;
	final messages = new Deque();

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
		if(handle == mainThread.handle) {
			return mainThread;
		}
		threadsMutex.acquire();
		var thread = null;
		for(item in threads) {
			if(item.handle == handle) {
				thread = item;
				break;
			}
		}
		if(thread == null) {
			thread = new HaxeThread(handle);
			threads.push(thread);
		}
		threadsMutex.release();
		return thread;
	}

	public static function create(callb:()->Void, withEventLoop:Bool):Thread {
		var item = new HaxeThread(null);
		threadsMutex.acquire();
		threads.push(item);
		threadsMutex.release();
		if(withEventLoop)
			item.events = new EventLoop();
		item.handle = createHandle(() -> {
			if(item.handle == null) {
				item.handle = currentHandle();
			}
			try {
				hl.Api.setErrorHandler(function(_){});
				callb();
				if(withEventLoop)
					item.events.loop();
			} catch(e) {
				hl.Api.setErrorHandler(null);
				dropThread(item);
				hl.Api.rethrow(e);
			}
			dropThread(item);
		});
		return item;
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

	public function new(h) {
		handle = h;
	}

	public function sendMessage(msg:Dynamic) {
		messages.add(msg);
	}
}
