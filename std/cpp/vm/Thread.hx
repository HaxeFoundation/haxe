/*
 * Copyright (c) 2005, The haXe Project Contributors
 * All rights reserved.
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 *   - Redistributions of source code must retain the above copyright
 *     notice, this list of conditions and the following disclaimer.
 *   - Redistributions in binary form must reproduce the above copyright
 *     notice, this list of conditions and the following disclaimer in the
 *     documentation and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE HAXE PROJECT CONTRIBUTORS "AS IS" AND ANY
 * EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE HAXE PROJECT CONTRIBUTORS BE LIABLE FOR
 * ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
 * SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
 * CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH
 * DAMAGE.
 */
package cpp.vm;

typedef ThreadHandle = Dynamic;

class Thread {

	var handle : ThreadHandle;

	function new(h) {
		handle = h;
	}

	/**
		Send a message to the thread queue. This message can be readed by using [readMessage].
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
		Creates a new thread that will execute the [callb] function, then exit.
	**/
	public static function create( callb : Void -> Void ) {
		return new Thread(thread_create(function(_) { callb(); },[]));
	}

	/**
		Reads a message from the thread queue. If [block] is true, the function
		blocks until a message is available. If [block] is false, the function
		returns [null] if no message is available.
	**/
	public static function readMessage( block : Bool ) : Dynamic {
		return thread_read_message(block);
	}

	function __compare(t) {
		return untyped handle == t.handle;
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
		[sync] returns immediatly. See [osInitialize] remarks.
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
		var v = new cpp.vm.Lock();
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
		os_loop = cpp.Lib.load("os","os_loop",0);
		os_loop_stop = cpp.Lib.load("os","os_loop_stop",1);
		os_sync = cpp.Lib.load("os","os_sync",2);
	}

	static var os_loop = null;
	static var os_loop_stop = null;
	static var os_sync = null;
	*/

	static var thread_create = cpp.Lib.load("std","thread_create",2);
	static var thread_current = cpp.Lib.load("std","thread_current",0);
	static var thread_send = cpp.Lib.load("std","thread_send",2);
	static var thread_read_message = cpp.Lib.load("std","thread_read_message",1);

}
