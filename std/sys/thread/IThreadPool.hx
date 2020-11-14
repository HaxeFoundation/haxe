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

/**
	A thread pool interface.
**/
@:using(sys.thread.IThreadPool.ThreadPoolUtils)
interface IThreadPool {

	/** Amount of alive threads in this pool. */
	var threadsCount(get,never):Int;

	/** Indicates if `shutdown` method of this pool has been called. */
	var isShutdown(get,never):Bool;

	/**
		Submit a task to run in a thread.

		Throws an exception if the pool is shut down.
	**/
	function run(task:()->Void):Void;

	/**
		Initiates a shutdown.
		All previousely submitted tasks will be executed, but no new tasks will
		be accepted.

		Multiple calls to this method have no effect.
	**/
	function shutdown():Void;
}

class ThreadPoolUtils {
	/**
		Run `task` and then invoke `callback` with the outcome of the task.

		The callback will be invoked in the same thread `.runFor` was called.
		The calling thread must have an event loop set up (see `sys.thread.Thread.events`)
	**/
	static public function runFor<R>(pool:IThreadPool, task:()->R, callback:haxe.Callback<haxe.Exception,R>):Void {
		var events = sys.thread.Thread.current().events;
		events.promise();
		pool.run(() -> {
			var result = try {
				task();
			} catch(e) {
				events.runPromised(() -> callback.fail(e));
				return;
			}
			events.runPromised(() -> callback.success(result));
		});
	}
}