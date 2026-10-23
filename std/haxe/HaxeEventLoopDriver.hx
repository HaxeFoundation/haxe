/*
 * Copyright (C)2005-2026 Haxe Foundation
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

package haxe;

/**
	Default `EventLoop` driver: threaded targets own a `sys.thread.Lock`;
	non-threaded sys uses `Sys.sleep` for positive timeouts; JS/Flash-shaped
	targets no-op `wait` / `wake`.
**/
class HaxeEventLoopDriver implements EventLoopDriver {
	public final allowsReentrancy:Bool = true;

	#if target.threaded
	final lock:sys.thread.Lock;
	#end

	var closed:Bool = false;

	public function new() {
		#if target.threaded
		lock = new sys.thread.Lock();
		#end
	}

	public function wait(maxBlock:Float):Void {
		if (closed || maxBlock < 0)
			return;
		#if target.threaded
		if (maxBlock == 0)
			lock.wait();
		else
			lock.wait(maxBlock);
		#elseif sys
		if (maxBlock > 0)
			Sys.sleep(maxBlock);
		// maxBlock == 0: best-effort no-op without threads
		#else
		// JS / Flash: guaranteed no-ops
		#end
	}

	public function wake():Void {
		if (closed)
			return;
		#if target.threaded
		lock.release();
		#end
	}

	public function close():Void {
		if (closed)
			return;
		closed = true;
		#if target.threaded
		// Unblock any waiter that may still be in wait(0)/wait(timeout)
		lock.release();
		#end
	}

	public function hasExternalWork():Bool {
		return false;
	}
}
