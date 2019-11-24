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

import haxe.Timer;
import cs.Lib;
import cs.system.threading.ManualResetEvent;

class Lock {
	final lockObj = {};
	final releaseEvent = new ManualResetEvent(false);

	var waitCount = 1; // initially locked
	var releaseCount = 0;

	public function new():Void {}

	public function wait(?timeout:Float):Bool {
		var myTicket;
		// Get a ticket in queue
		Lib.lock(lockObj, {
			myTicket = waitCount;
			waitCount++;
			if (myTicket <= releaseCount) {
				return true;
			}
			releaseEvent.Reset();
		});

		if (timeout == null) {
			do {
				releaseEvent.WaitOne();
				if (myTicket <= releaseCount) {
					return true;
				}
			} while (true);
		} else {
			var timeoutStamp = Timer.stamp() + timeout;
			do {
				var secondsLeft = timeoutStamp - Timer.stamp();
				if (secondsLeft <= 0 || !releaseEvent.WaitOne(Std.int(secondsLeft * 1000))) {
					// Timeout. Do not occupy a place in queue anymore
					release();
					return false;
				}
				if (myTicket <= releaseCount) {
					return true;
				}
			} while (true);
		}
	}

	public function release():Void {
		Lib.lock(lockObj, {
			releaseCount++;
			releaseEvent.Set();
		});
	}
}
