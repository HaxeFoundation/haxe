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

package hl.uv;

import hl.uv.Handle;

/**
	Async handles allow the user to “wakeup” the event loop
	and get a callback called from another thread.

	@see http://docs.libuv.org/en/v1.x/async.html
**/
class Async extends Handle<RefUvAsyncT> {
	var onSend:(async:Async)->Void;

	/**
		Allocate and initialize the handle.
	**/
	static public function init(loop:Loop, callback:(async:Async)->Void):Async {
		loop.checkLoop();
		var async = new Async(UV.alloc_async());
		var result = loop.async_init_with_cb(async.h);
		if(result < 0) {
			async.freeHandle();
			result.throwErr();
		}
		async.onSend = callback;
		return async;
	}

	/**
		Wake up the event loop and call the async handle’s callback on the loop's thread.
	**/
	public function send():Void {
		handle(h -> h.async_send().resolve());
	}
}