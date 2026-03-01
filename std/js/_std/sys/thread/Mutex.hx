/*
 * Copyright (C)2005-2024 Haxe Foundation
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

import js.lib.Atomics;

@:noPackageRestrict
class Mutex {
	// 0 = unlocked, 1 = locked
	final _state:js.lib.Int32Array;

	public function new():Void {
		_state = new js.lib.Int32Array(new js.lib.SharedArrayBuffer(js.lib.Int32Array.BYTES_PER_ELEMENT));
	}

	public function acquire():Void {
		while (Atomics.compareExchange(_state, 0, 0, 1) != 0) {
			// In workers, wait efficiently until notified.
			// On the main thread, Atomics.wait throws, so spin in that case.
			try {
				Atomics.wait(_state, 0, 1);
			} catch (_:Dynamic) {}
		}
	}

	public function tryAcquire():Bool {
		return Atomics.compareExchange(_state, 0, 0, 1) == 0;
	}

	public function release():Void {
		Atomics.store(_state, 0, 0);
		Atomics.notify(_state, 0, 1);
	}
}
