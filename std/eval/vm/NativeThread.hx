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

package eval.vm;

extern class NativeThread {
	/**
		Creates a new thread that executes function `f`.

		Exceptions caused while executing `f` are printed to stderr and are not
		propagated to the parent thread.
	**/
	function new(f:Void->Void):Void;

	/**
		Return the identifier of the given thread. A thread identifier is an integer
		that identifies uniquely the thread. It can be used to build data structures
		indexed by threads.
	**/
	function id():Int;

	/**
		Terminate prematurely the thread whose handle is given. This functionality is
		available only with bytecode-level threads.
	**/
	function kill():Int;

	/**
		Suspends the execution of the calling thread for `f` seconds. The other program
		threads continue to run during this time.
	**/
	static function delay(f:Float):Void;

	/**
		Terminate prematurely the currently executing thread.
	**/
	static function exit():Void;

	/**
		Suspends the execution of the calling thread until the thread `thread` has
		terminated.
	**/
	static function join(thread:NativeThread):Void;

	/**
		Return the thread currently executing.
	**/
	static function self():NativeThread;

	/**
		Re-schedule the calling thread without suspending it. This function can be used
		to give scheduling hints, telling the scheduler that now is a good time to switch
		to other threads.
	**/
	static function yield():Void;

	static function readMessage<T>(block:Bool):T;

	function sendMessage<T>(msg:T):Void;

	@:allow(sys.thread.Thread)
	private var events(get,set):Null<sys.thread.EventLoop>;
}
