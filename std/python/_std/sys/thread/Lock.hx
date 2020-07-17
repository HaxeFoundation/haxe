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

import python.Exceptions.InterruptedError;

class Lock {
	var releasedCount = 0;
	var waitLock: NativeLock;
	var releaseLock: NativeLock;
	var condition: NativeCondition;

	public function new() {
		waitLock = new NativeLock();
		releaseLock = new NativeLock();
		condition = new NativeCondition();
	}

	// TODO use python Condition object to do the wait()/notify() thing that
	// the Java lock implementation does.

	public function wait(?timeout:Float):Bool {
		trace('wait $timeout called from ${haxe.CallStack.callStack().toString()}');
		var ret = false;
		waitLock.acquire(true);
		if (--releasedCount < 0) {
			if (timeout == null) {
				while (releasedCount < 0) {
					try {
						trace(releasedCount);
						condition.acquire();
						condition.wait();
						condition.release();
					} catch (e: InterruptedError) {}
				}
			} else {
				var timeout = timeout * 1000;
				var cur = Sys.time(),
					max = cur + timeout;
				while (releasedCount < 0 && cur < max) {
					try {
						var t = max - cur;
						condition.acquire();
						condition.wait(t / 1000.);
						condition.release();
						cur = Sys.time();
					} catch (e: InterruptedError) {}
				}
 			}
		}

		ret = releasedCount >= 0;
		if (!ret)
			releasedCount++;
		waitLock.release();
		return ret;
	}

	/**
		Releases the lock once.

		The thread does not need to own the lock in order to release
		it. Each call to `release` allows exactly one call to `wait`
		to execute.
	**/
	public function release():Void {
		trace('release called from ${haxe.CallStack.callStack().toString()}');
		releaseLock.acquire(true);
		++releasedCount;
		trace(releasedCount);
		if (releasedCount == 0) {
			condition.acquire();
			condition.notify();
			condition.release();
		}
		releaseLock.release();
	}
}

@:pythonImport("threading", "Lock")
@:native("Lock")
private extern class NativeLock {
	function new();
	function acquire(blocking: Bool=true, ?timeout: Float=-1):Bool;
	function release():Void;
}

@:pythonImport("threading", "Condition")
@:native("Condition")
private extern class NativeCondition {
	function new();
	function acquire():Void;
	function release():Void;
	function wait(?timeout: Float):Void;
	function notify():Void;
}