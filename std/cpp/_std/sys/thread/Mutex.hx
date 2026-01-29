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

@:include("hx/thread/RecursiveMutex.hpp")
@:cpp.ManagedType({ namespace : [ "hx", "thread" ], flags : [ StandardNaming ] })
private extern class RecursiveMutex {
	function new():Void;

	function acquire():Void;
	function tryAcquire():Bool;
	function release():Void;
}

@:coreApi
class Mutex {
	final m:RecursiveMutex;

	public function new() {
		m = new RecursiveMutex();
	}

	public function acquire():Void {
		m.acquire();
	}

	public function tryAcquire():Bool {
		return m.tryAcquire();
	}

	public function release():Void {
		m.release();
	}
}
