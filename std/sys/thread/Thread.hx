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

typedef CurrentThreadCallbacks = {
	/**
		Called when the thread has successfully completed its job. Not called if the thread throws.
	**/
	?onJobDone:() -> Void,
	/**
		Called when an uncaught exception aborts the thread. If this callback throws, the exception
		is forwarded to the default abort handler and `onExit` callbacks are still called.
	**/
	?onAbort:haxe.Exception -> Void,
	/**
		Called when the thread is exiting, after `onAbort` if applicable. If this callback throws,
		the exception is forwarded to the default abort handler, ignoring any assigned `onAbort`.
	**/
	?onExit:() -> Void
}

typedef ThreadCallbacks = CurrentThreadCallbacks & {
	/**
		Called from the creating thread, synchronously within `Thread.create`, just before the
		new thread is actually spawned. Unlike `onStart`, this fires in the context of the
		creating thread, so `Thread.current()` returns the creating thread, not the new thread.
	**/
	?onCreate:() -> Void,
	/**
		Called when the thread starts, before the job is executed.
	**/
	?onStart:() -> Void
}

class Thread {

	static var threads : Array<Thread>;
	static var mutex : Mutex;
	static var mainThread : Thread;
	static var idCounter : Int; // TODO: Should probably be an AtomicInt
	static var globalCallbacks : ThreadCallbackManager;

	@:deprecated("Use haxe.EventLoop.getThreadLoop(thread) instead")
	public var events(get, null):Null<haxe.EventLoop>;

	inline function get_events() {
		return haxe.EventLoop.getThreadLoop(this);
	}

	public final id : Int;
	var impl : ThreadImpl;
	var messages : Deque<Dynamic>;
	final callbacks : ThreadCallbackManager;

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
		callbacks = new ThreadCallbackManager();
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

	static function installCallbacks(host:ThreadCallbackManager, callbacks:ThreadCallbacks) {
		final handles:Array<IThreadCallbackHandle> = [];
		if (callbacks.onCreate != null) {
			handles.push(host.onCreate(callbacks.onCreate));
		}
		if (callbacks.onStart != null) {
			handles.push(host.onStart(callbacks.onStart));
		}
		if (callbacks.onJobDone != null) {
			handles.push(host.onJobDone(callbacks.onJobDone));
		}
		if (callbacks.onAbort != null) {
			handles.push(host.onAbort(callbacks.onAbort));
		}
		if (callbacks.onExit != null) {
			handles.push(host.onExit(callbacks.onExit));
		}
		if (handles.length == 1) {
			return handles[0];
		}
		return new MultiHandle(handles);
	}

	/**
		Creates a new thread that will execute the `job` function, then exit after all events are processed.
		You can specify a custom exception handler `onAbort` or else `Thread.onAbort` will be called.
	**/
	public static function create(?name:String, job:()->Void, ?callbacks:ThreadCallbacks):Thread {
		mutex.acquire();
		var t = new Thread(null);
		threads.push(t);
		mutex.release();

		if (callbacks != null) {
			if (callbacks.onAbort == null) {
				callbacks.onAbort = t.onAbort;
			}
			installCallbacks(t.callbacks, callbacks);
		} else {
			t.callbacks.onAbort(t.onAbort);
		}
		ThreadCallbackManager.invokeCallbacks(t.callbacks.onCreateCallback, globalCallbacks?.onCreateCallback);
		t.impl = ThreadImpl.create(function() {
			t.impl = ThreadImpl.current();
			if( name != null ) t.name = name;
			currentTLS.value = t;
			var exception = null;
			try {
				#if hl
				hl.Api.setErrorHandler(null);
				#end
				ThreadCallbackManager.invokeCallbacks(t.callbacks.onStartCallback, globalCallbacks?.onStartCallback);
				job();
				ThreadCallbackManager.invokeCallbacks(t.callbacks.onJobDoneCallback, globalCallbacks?.onJobDoneCallback);
			}
			#if eval
			catch (_:eval.vm.NativeThread.NativeThreadExit) {
				// This comes from a NativeThread.exit() call and is not a real exception
			}
			#end
			catch( e ) {
				exception = e;
			}

			if( exception != null ) {
				try {
					ThreadCallbackManager.invokeCallbacksArg(t.callbacks.onAbortCallback, globalCallbacks?.onAbortCallback, exception);
				} catch ( e ) {
					t.onAbort(e);
				}
			}

			try {
				ThreadCallbackManager.invokeCallbacks(t.callbacks.onExitCallback, globalCallbacks?.onExitCallback);
			} catch ( e ) {
				t.onAbort(e);
			}

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
		Registers `callbacks` to be called for every thread, both already-running and future ones.
		Returns a handle that can be used to unregister the callbacks.

		Unlike callbacks passed to `Thread.create`, closing the returned handle prevents the
		callbacks from being called even for threads that are already running.
	**/
	static public function addCallbacks(callbacks:ThreadCallbacks):IThreadCallbackHandle {
		globalCallbacks ??= new ThreadCallbackManager();
		return installCallbacks(globalCallbacks, callbacks);
	}

	/**
		Registers `callbacks` to be called for the current thread.
	**/
	static public function addCurrentCallbacks(callbacks:CurrentThreadCallbacks):IThreadCallbackHandle {
		final thread = Thread.current();
		return installCallbacks(thread.callbacks, {
			onStart: null,
			onJobDone: callbacks.onJobDone,
			onAbort: callbacks.onAbort,
			onExit: callbacks.onExit
		});
	}

	/**
		This function is called when an uncaught exception aborted a thread.
		The error will be printed to stdout but this function can be redefined.

		If this function throws, the exception is forwarded to the default handler
		(print to stdout) and `onExit` callbacks are still called.

		It is generally good practice to call any previously existing callback
		from functions assigned to this.
	**/
	function onAbort(e:haxe.Exception) {
		var name = this.name;
		if( name == null ) name = "" else name = " "+name;
		Sys.println("THREAD"+name+" ABORTED : "+e.message+haxe.CallStack.toString(e.stack));
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
