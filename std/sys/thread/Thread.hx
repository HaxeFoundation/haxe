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

extern abstract Thread({}) {
	/**
		Send a message to the thread queue. This message can be read by using `readMessage`.
	**/
	public function sendMessage(msg:Dynamic):Void;

	/**
		Returns the current thread.
	**/
	public static function current():Thread;

	/**
		Creates a new thread that will execute the `job` function, then exit.
	**/
	public static function create(job:Void->Void):Thread;

	/**
		Reads a message from the thread queue. If `block` is true, the function
		blocks until a message is available. If `block` is false, the function
		returns `null` if no message is available.
	**/
	public static function readMessage(block:Bool):Dynamic;

	/**
		Schedule event for execution every `intervalMs` milliseconds in current thread.
	**/
	public static function repeatEvent(event:()->Void, intervalMs:Int):EventHandler;

	/**
		Prevent execution of a previousely scheduled event in current thread.
	**/
	public static function cancelEvent(eventHandler:EventHandler):Void;

	/**
		Notify this thread about an upcoming event.
		This makes the thread to stay alive and wait for as many events as many times
		`thread.promiseEvent()` was called. These events should be added via
		`thread.runPromisedEvent()`
	**/
	public function promiseEvent():Void;

	/**
		Execute `event` as soon as possible after this thread finished its job.

		Note that events are not guaranteed to be processed if the thread was
		created using target native API instead of `sys.thread.Thread.create`
		(except the main thread).
	**/
	public function runEvent(event:()->Void):Void;

	/**
		Add previously promised `event` for execution after this thread finished its job.
	**/
	public function runPromisedEvent(event:()->Void):Void;

	/**
		Execute all pending events.
		Wait and execute as many events as many times `Thread.eventComingUp()` was called.
	**/
	private static function processEvents():Void;
}

@:coreType abstract EventHandler {}