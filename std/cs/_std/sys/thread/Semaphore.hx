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
class Semaphore {
	var _semaphore:Dynamic; // System.Threading.SemaphoreSlim

	public function new(value:Int):Void {
		_semaphore = untyped __cs__("new System.Threading.SemaphoreSlim({0}, int.MaxValue)", value);
	}

	public function acquire():Void {
		untyped __cs__("{0}.Wait()", _semaphore);
	}

	public function tryAcquire(?timeout:Float):Bool {
		if (timeout == null) {
			return untyped __cs__("{0}.Wait(0)", _semaphore);
		} else {
			var timeoutMs:Int = Std.int(timeout * 1000.0);
			return untyped __cs__("{0}.Wait({1})", _semaphore, timeoutMs);
		}
	}

	public function release():Void {
		untyped __cs__("{0}.Release()", _semaphore);
	}
}
