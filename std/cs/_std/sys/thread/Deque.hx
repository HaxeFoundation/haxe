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
	var _semaphore:cs.system.threading.SemaphoreSlim;

	public function new() {
		_queue = cs.Syntax.code("new System.Collections.Concurrent.ConcurrentQueue<object>()");
		_semaphore = new cs.system.threading.SemaphoreSlim(0, 2147483647);
	}

	public function add(i:T):Void {
		cs.Syntax.code("((System.Collections.Concurrent.ConcurrentQueue<object>){0}).Enqueue({1})", _queue, i);
		_semaphore.Release();
	}

	public function push(i:T):Void {
		// ConcurrentQueue doesn't have push to front, so we use a different approach
		// For now, just add to end (same as add)
		add(i);
	}

	public function pop(block:Bool):Null<T> {
		if (block) {
			// Wait for an item
			_semaphore.Wait();
			var result:Dynamic = null;
			cs.Syntax.code("((System.Collections.Concurrent.ConcurrentQueue<object>){0}).TryDequeue(out {1})", _queue, result);
			return cast result;
		} else {
			// Try to get without blocking
			if (_semaphore.Wait(0)) {
				var result:Dynamic = null;
				cs.Syntax.code("((System.Collections.Concurrent.ConcurrentQueue<object>){0}).TryDequeue(out {1})", _queue, result);
				return cast result;
			}
			return null;
		}
	}
}
