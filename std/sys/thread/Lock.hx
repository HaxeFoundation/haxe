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

/**
	A Lock allows blocking execution until it has been unlocked. It keeps track
	of how often `release` has been called, and blocks exactly as many `wait`
	calls.

	The order of the `release` and `wait` calls is irrelevant. That is, a Lock
	can be released before anyone waits for it. In that case, the `wait` call
	will execute immediately.

	Usage example:

	```haxe
	var lock = new Lock();
	var elements = [1, 2, 3];
	for (element in elements) {
		// Create one thread per element
		new Thread(function() {
			trace(element);
			Sys.sleep(1);
			// Release once per thread = 3 times
			lock.release();
		});
	}
	for (_ in elements) {
		// Wait 3 times
		lock.wait();
	}
	trace("All threads finished");
	```
**/
extern class Lock {
	/**
		Creates a new Lock which is initially locked.
	**/
	function new():Void;

	/**
		Waits for the lock to be released, or `timeout` (in seconds)
		to expire. Returns `true` if the lock is released and `false`
		if a time-out occurs.
	**/
	function wait(?timeout:Float):Bool;

	/**
		Releases the lock once.

		The thread does not need to own the lock in order to release
		it. Each call to `release` allows exactly one call to `wait`
		to execute.
	**/
	function release():Void;
}
