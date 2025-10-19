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

class Thread {

	static var threads : Array<Thread>;
	static var mutex : Mutex;
	static var mainThread : Thread;

	var impl : ThreadImpl;

	/**
		Event loop of this thread.
	**/
	public var events(default,null) : haxe.EventLoop;

	/**
		Tells if we needs to wait for the thread to terminate before we stop the main loop (default:true).
	**/
	public var isBlocking : Bool = true;

	/**
		Allows to query or change the name of the thread. On some platforms this might allow debugger to identify threads.
	**/
	public var name(default,set) : Null<String>;

	function new(impl) {
		this.impl = impl;
		if( impl != null ) this.name = ThreadImpl.getName(impl);
	}

	function set_name(n) {
		name = n;
		if( impl != null ) ThreadImpl.setName(impl,name == null ? "" : name);
		return n;
	}

	/**
		Returns the current thread.
	**/
	public static function current():Thread {
		var impl = ThreadImpl.current();
		if( impl == mainThread.impl )
			return mainThread;
		mutex.acquire();
		for( t in threads )
			if( t.impl == impl ) {
				mutex.release();
				return t;
			}
		var t = new Thread(impl);
		// keep t.events = null because this is an unkown thread (not main and not created with create())
		threads.push(t);
		mutex.release();
		return t;
	}

	/**
		Returns the main thread
	**/
	public static inline function main() {
		return mainThread;
	}

	/**
		Creates a new thread that will execute the `job` function, then exit after all events are processed.
		You can specify a custom exception handler `onAbort` or else `Thread.onAbort` will be called.
	**/
	public static function create(job:()->Void,?onAbort):Thread {
		mutex.acquire();
		var t = new Thread(null);
		t.events = new haxe.EventLoop();
		threads.push(t);
		mutex.release();
		if( onAbort != null )
			t.onAbort = onAbort;
		t.impl = ThreadImpl.create(function() {
			t.impl = ThreadImpl.current();
			var exception = null;
			try {
				job();
				t.events.loop();
			} catch( e ) {
				exception = e;
			}
			mutex.acquire();
			threads.remove(t);
			mutex.release();
			if( exception != null )
				t.onAbort(exception);
		});
		return t;
	}

	/**
		This function is called when an uncaught exception aborted a thread.
		The error will be printed to stdout but this function can be redefined.
	**/
	public dynamic function onAbort(e:haxe.Exception) {
		var name = this.name;
		if( name == null ) name = "" else name = " "+name;
		Sys.println("THREAD"+name+" ABORTED : "+e.message+haxe.CallStack.toString(e.stack));
	}

	static function hasBlocking() {
		// let's check if we have blocking threads running
		mutex.acquire();
		for( t in threads )
			if( t.isBlocking ) {
				mutex.release();
				return true;
			}
		mutex.release();
		return false;
	}

	static function __init__() {
		mutex = new Mutex();
		threads = [];
		mainThread = new Thread(ThreadImpl.current());
		mainThread.events = haxe.EventLoop.main;
	}

}
