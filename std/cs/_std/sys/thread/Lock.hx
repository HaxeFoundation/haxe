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

@:coreApi
class Lock {
	var _semaphore:cs.system.threading.SemaphoreSlim;

	public function new() {
		// SemaphoreSlim with initial count 0 and max count Int32.MaxValue
		_semaphore = new cs.system.threading.SemaphoreSlim(0, 2147483647);
	}

	public function wait(?timeout:Float):Bool {
		if (timeout == null) {
			// Wait indefinitely
			_semaphore.Wait();
			return true;
		} else {
			// Wait with timeout in milliseconds
			var timeoutMs:Int = Std.int(timeout * 1000.0);
			return _semaphore.Wait(timeoutMs);
		}
	}

	public function release():Void {
		_semaphore.Release();
	}
}
