/*
 * Copyright (C)2005-2017 Haxe Foundation
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
package neko.vm;

enum ThreadHandle {
}

class Thread {

	var handle : ThreadHandle;

	function new(h) {
		handle = h;
	}

	/**
		Send a message to the thread queue. This message can be read by using `readMessage`.
	**/
	public function sendMessage( msg : Dynamic ) {
		thread_send(handle,msg);
	}


	/**
		Returns the current thread.
	**/
	public static function current() {
		return new Thread(thread_current());
	}

	/**
		Creates a new thread that will execute the `callb` function, then exit.
	**/
	public static function create( callb : Void -> Void ) {
		return new Thread(thread_create(function(_) { return callb(); },null));
	}

	/**
		Reads a message from the thread queue. If `block` is true, the function
		blocks until a message is available. If `block` is false, the function
		returns `null` if no message is available.
	**/
	public static function readMessage( block : Bool ) : Dynamic {
		return thread_read_message(block);
	}

	@:keep function __compare(t) {
		return untyped __dollar__compare(handle,t.handle);
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

	static var thread_create = neko.Lib.load("std","thread_create",2);
	static var thread_current = neko.Lib.load("std","thread_current",0);
	static var thread_send = neko.Lib.load("std","thread_send",2);
	static var thread_read_message = neko.Lib.load("std","thread_read_message",1);

}
