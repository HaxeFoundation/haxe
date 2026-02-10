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
 * Lock implementation using BlockingCollection (like JVM uses LinkedBlockingDeque).
 * This provides reliable cross-thread signaling for EventLoop.
 */
@:coreApi
class Lock {
	var _queue:Dynamic; // BlockingCollection<int>

	public function new() {
		_queue = cs.Syntax.code("new global::System.Collections.Concurrent.BlockingCollection<int>()");
	}

	public function wait(?timeout:Float):Bool {
		if (timeout == null) {
			// Wait indefinitely - Take blocks until an item is available
			cs.Syntax.code("((global::System.Collections.Concurrent.BlockingCollection<int>){0}).Take()", _queue);
			return true;
		} else {
			// Wait with timeout in milliseconds
			// Cap timeout to avoid integer overflow (max ~24 days in ms)
			var timeoutMs:Int = timeout > 2147483.0 ? 2147483647 : Std.int(timeout * 1000.0);
			if (timeoutMs < 0)
				timeoutMs = 0;
			// TryTake with out parameter - use inline declaration
			var result:Bool = cs.Syntax.code("((global::System.Collections.Concurrent.BlockingCollection<int>){0}).TryTake(out _, {1})", _queue, timeoutMs);
			return result;
		}
	}

	public function release():Void {
		cs.Syntax.code("((global::System.Collections.Concurrent.BlockingCollection<int>){0}).Add(0)", _queue);
	}
}
