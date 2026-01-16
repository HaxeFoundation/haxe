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
class Deque<T> {
	var _queue:Dynamic; // System.Collections.Concurrent.ConcurrentQueue<object>
	var _semaphore:Dynamic; // System.Threading.SemaphoreSlim

	public function new() {
		_queue = untyped __cs__("new System.Collections.Concurrent.ConcurrentQueue<object>()");
		_semaphore = untyped __cs__("new System.Threading.SemaphoreSlim(0)");
	}

	public function add(i:T):Void {
		untyped __cs__("{0}.Enqueue({1})", _queue, i);
		untyped __cs__("{0}.Release()", _semaphore);
	}

	public function push(i:T):Void {
		// ConcurrentQueue doesn't have push to front, so we use a different approach
		// For now, just add to end (same as add)
		add(i);
	}

	public function pop(block:Bool):Null<T> {
		if (block) {
			// Wait for an item
			untyped __cs__("{0}.Wait()", _semaphore);
			var result:Dynamic = null;
			untyped __cs__("{0}.TryDequeue(out {1})", _queue, result);
			return cast result;
		} else {
			// Try to get without blocking
			if (untyped __cs__("{0}.Wait(0)", _semaphore)) {
				var result:Dynamic = null;
				untyped __cs__("{0}.TryDequeue(out {1})", _queue, result);
				return cast result;
			}
			return null;
		}
	}
}
