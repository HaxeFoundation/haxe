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

using hl.uv.UV;

/**
	Idle handles will run the given callback once per loop iteration,
	right before the `hl.uv.Prepare` handles.

	@see http://docs.libuv.org/en/v1.x/idle.html
**/
class Idle extends Handle<UvIdleTStar> {
	var callback:()->Void;

	/**
		Allocate and initialize the handle.
	**/
	static public function init(loop:Loop):Idle {
		loop.checkLoop();
		var idle = new Idle(UV.alloc_idle());
		var result = loop.idle_init(idle.h);
		if(result < 0) {
			idle.freeHandle();
			result.throwErr();
		}
		return idle;
	}

	/**
		Start the handle with the given callback.
	**/
	public function start(callback:()->Void):Void {
		handle(h -> {
			h.idle_start_with_cb().resolve();
			this.callback = callback;
		});
	}

	/**
		Stop the handle, the callback will no longer be called.
	**/
	public function stop():Void {
		handle(h -> h.idle_stop().resolve());
	}
}