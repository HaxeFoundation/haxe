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

class Lock {
	final lockObj = {};
	/** If `queue` is greater than `0` it means `release` was called more times than `wait` */
	var queue:Int = 0;

	public function new():Void {}

	public function wait(?timeout:Float):Bool {
		var timeoutStamp = timeout == null ? 0 : Timer.stamp() + timeout;
		inline function timedOut() {
			return timeout != null && Timer.stamp() < timeoutStamp;
		}

		var myTicket;
		//Get a ticket in queue
		Lib.lock(lockObj, {
			myTicket = queue;
			queue--;
		})

		//Waiting for our ticket to be reached by `release` calls
		do {
			Lib.lock(lockObj, {
				if(queue >= myTicket || queue > 0) {
					return true;
				}
			});
		} while(!timedOut());

		//Do not occupy a place in queue any more
		Lib.lock(lockObj, {
			queue++;
		});
		return false;
	}

	public function release():Void {
		Lib.lock(lockObj, {
			queue++;
		});
	}
}
