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

#if (!target.threaded)
#error "This class is not available on this target"
#end

import sys.thread.ThreadCallback;

private typedef ThreadCreateCallbacks = {
	?onJobDone:() -> Void,
	?onAbort:haxe.Exception -> Void,
	?onExit:() -> Void
}

class Thread {

	static var threads : Array<Thread>;
	static var mutex : Mutex;
	static var mainThread : Thread;
	static var idCounter : Int; // TODO: Should probably be an AtomicInt
	static var onJobStartCallback : ThreadCallbacks<ThreadInstanceCallbacks -> Void>;

	@:deprecated("Use haxe.EventLoop.getThreadLoop(thread) instead")
	public var events(get, null):Null<haxe.EventLoop>;

	inline function get_events() {
		return haxe.EventLoop.getThreadLoop(this);
	}

	public final id : Int;
	var impl : ThreadImpl;
	var messages : Deque<Dynamic>;
	final callbacks : ThreadInstanceCallbacks;

	/**
		Tells if we needs to wait for the thread to terminate before we stop the main loop (default:true).
	**/
	public var isBlocking : Bool = true;

	/**
		Allows to query or change the name of the thread. On some platforms this might allow debugger to identify threads.
	**/
	public var name(default,set) : Null<String>;

	/**
		Tells if a thread is a native thread that is not managed by Haxe.
		See `Thread.current` for details.
	**/
	public var isNative(default,null) : Bool;

	function new(impl) {
		this.id = idCounter++;
		this.impl = impl;
		if( impl != null ) this.name = ThreadImpl.getName(impl);
		callbacks = new ThreadInstanceCallbacks();
	}

	function set_name(n) {
		name = n;
		if( impl != null ) ThreadImpl.setName(impl,name == null ? "" : name);
		return n;
	}

	public function toString() {
		return "Thread#"+(name ?? Std.string(impl));
	}

	public function sendMessage( msg : Dynamic ) {
		if( messages == null ) {
			mutex.acquire();
			if( messages == null ) messages = new Deque();
			mutex.release();
		}
		messages.add(msg);
	}

	public function disposeNative() {
		if( !isNative ) return;
		dispose();
	}


	function dispose() {
		mutex.acquire();
		threads.remove(this);
		mutex.release();
		currentTLS.value = null;
	}

	public static function readMessage( blocking : Bool ) : Null<Dynamic> {
		var t = current();
		if( t.messages == null ) {
			mutex.acquire();
			if( t.messages == null ) t.messages = new Deque();
			mutex.release();
		}
		return t.messages.pop(blocking);
	}

	static var currentTLS : Tls<Thread>;

	/**
		Returns the current thread.
		If you are calling this function from a native thread that is not the main thread and was not created by `Thread.create`, this will return you
		a native thread with a `null` EvenLoop and `isNative` set to true. You need to call `disposeNative()` on such value on thread termination.
	**/
	public static function current():Thread {
		var t = currentTLS.value;
		if( t != null )
			return t;
		var impl = ThreadImpl.current();
		var t = new Thread(impl);
		t.isNative = true;
		mutex.acquire();
		threads.push(t);
		mutex.release();
		currentTLS.value = t;
		return t;
	}

	/**
		Returns the main thread
	**/
	public static inline function main() {
		return mainThread;
	}

	function installCallbacks(callbacks:ThreadCreateCallbacks) {
		if (callbacks.onJobDone != null) {
			this.callbacks.onJobDone(callbacks.onJobDone);
		}
		if (callbacks.onAbort != null) {
			// TODO: Change to variable too?
			onAbort = callbacks.onAbort;
		}
		if (callbacks.onExit != null) {
			this.callbacks.onExit(callbacks.onExit);
		}
	}

	/**
		Creates a new thread that will execute the `job` function, then exit after all events are processed.
		You can specify a custom exception handler `onAbort` or else `Thread.onAbort` will be called.
	**/
	public static function create(?name:String, job:()->Void, ?callbacks:ThreadCreateCallbacks):Thread {
		mutex.acquire();
		var t = new Thread(null);
		threads.push(t);
		mutex.release();
		if (callbacks != null) {
			t.installCallbacks(callbacks);
		}
		t.impl = ThreadImpl.create(function() {
			t.impl = ThreadImpl.current();
			if( name != null ) t.name = name;
			currentTLS.value = t;
			var exception = null;
			try {
				#if hl
				hl.Api.setErrorHandler(null);
				#end
				if (onJobStartCallback != null) {
					onJobStartCallback.foreach(f -> f(t.callbacks));
				}
				job();
				@:privateAccess t.callbacks.callOnJobDone();
			} catch( e ) {
				exception = e;
			}
			if( exception != null )
				t.onAbort(exception);
			@:privateAccess t.callbacks.callOnExit();
			t.dispose();
		});
		if( name != null ) t.name = name;
		return t;
	}

	/**
		Returns a list of all currently running threads.
		This excludes native threads which were created without Thread.create and have not been
		registered with a call to Thread.current().
	**/
	public static function getAll() {
		mutex.acquire();
		var tl = threads.copy();
		mutex.release();
		return tl;
	}

	/**
		Registers `f` to be called when a thread is about to start executing its job.
	**/
	static public function onJobStart(f:ThreadInstanceCallbacks -> Void):IThreadCallbackHandle {
		onJobStartCallback ??= new ThreadCallbacks();
		return onJobStartCallback.add(f);
	}

	/**
		Registers `f` to be called when the current thread exits.
		Returns a handle that can be used to unregister the callback.
	**/
	static public function onCurrentExit(f:() -> Void):IThreadCallbackHandle {
		return current().callbacks.onExit(f);
	}

	/**
		This function is called when an uncaught exception aborted a thread.
		The error will be printed to stdout but this function can be redefined.

		It is generally good practice to call any previously existing callback
		from functions assigned to this.
	**/
	dynamic function onAbort(e:haxe.Exception) {
		var name = this.name;
		if( name == null ) name = "" else name = " "+name;
		Sys.println("THREAD"+name+" ABORTED : "+e.message+haxe.CallStack.toString(e.stack));
	}

	static function hasBlocking() {
		// let's check if we have blocking threads running other that our calling thread
		var me = current();
		mutex.acquire();
		for( t in threads )
			if( t.impl != me.impl && t.isBlocking ) {
				mutex.release();
				return true;
			}
		mutex.release();
		return false;
	}

	static function __init__() {
		mutex = new Mutex();
		idCounter = 1;
		mainThread = new Thread(ThreadImpl.current());
		mainThread.name = "Main";
		threads = [mainThread];
		currentTLS = new Tls();
		currentTLS.value = mainThread;
	}

}
