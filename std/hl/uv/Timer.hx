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

using hl.uv.UV;

/**
	Timers.

	@see http://docs.libuv.org/en/v1.x/timer.html
**/
class Timer extends Handle<UvTimerTStar> {

	@:keep var callback:()->Void;

	/** The timer repeat value. */
	public var repeat(get,set):I64;

	function get_repeat():I64
		return handleReturn(h -> h.timer_get_repeat());

	function set_repeat(v:I64):I64 {
		handle(h -> h.timer_set_repeat(v));
		return v;
	}

	/** Get the timer due value or 0 if it has expired. */
	public var dueIn(get,never):I64;

	function get_dueIn():I64
		return handleReturn(h -> h.timer_get_due_in());

	/**
		Initialize the timer.
	**/
	static public function init(loop:Loop):Timer {
		loop.checkLoop();
		var timer = new Timer(UV.alloc_timer());
		var result = loop.timer_init(timer.h);
		if(result < 0) {
			timer.freeHandle();
			result.throwErr();
		}
		return timer;
	}

	/**
		Start the timer.
		`timeout` and `repeat` are in milliseconds.

		If `timeout` is zero, the `callback` fires on the next event loop iteration.
		If `repeat` is non-zero, the `callback` fires first after `timeout` milliseconds
		and then repeatedly after `repeat` milliseconds.

		TODO: change `timeout` and `repeat` to I64
	**/
	public function start(callback:()->Void, timeout:I64, repeat:I64):Void {
		handle(h -> {
			h.timer_start_with_cb(timeout, repeat).resolve();
			this.callback = callback;
		});
	}

	/**
		Stop the timer, the callback will not be called anymore.
	**/
	public function stop():Void {
		handle(h -> h.timer_stop().resolve());
	}

	/**
		Stop the timer, and if it is repeating restart it using the repeat value
		as the timeout.
		If the timer has never been started before it throws UV_EINVAL.
	**/
	public function again():Void {
		handle(h -> h.timer_again().resolve());
	}
}