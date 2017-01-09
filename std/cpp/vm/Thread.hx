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
package cpp.vm;

typedef ThreadHandle = Dynamic;

class Thread {

	public var handle(default,null) : ThreadHandle;

	function new(h) {
		handle = h;
	}

	/**
		Send a message to the thread queue. This message can be read by using `readMessage`.
	**/
	public function sendMessage( msg : Dynamic ) {
		untyped __global__.__hxcpp_thread_send(handle,msg);
	}


	/**
		Returns the current thread.
	**/
	public static function current() {
		return new Thread(untyped __global__.__hxcpp_thread_current());
	}

	/**
		Creates a new thread that will execute the `callb` function, then exit.
	**/
	public static function create( callb : Void -> Void ) {
		return new Thread(untyped __global__.__hxcpp_thread_create(callb));
	}

	/**
		Reads a message from the thread queue. If `block` is true, the function
		blocks until a message is available. If `block` is false, the function
		returns `null` if no message is available.
	**/
	public static function readMessage( block : Bool ) : Dynamic {
		return untyped __global__.__hxcpp_thread_read_message(block);
	}

	@:keep function __compare(t) : Int {
		return handle == t.handle ? 0 : 1;
	}

}

