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

package cpp.uv;

using cpp.uv.UV;

/**
	Timer handles are used to schedule callbacks to be called in the future.

	@see http://docs.libuv.org/en/v1.x/timer.html
**/
@:headerCode('#include "uv.h"')
class Timer extends Handle {
	var uvTimer:Star<UvTimerT>;
	var onTick:()->Void;

	function initUvHandle() {
		uvTimer = UvTimerT.create();
		uvHandle = cast uvTimer;
	}

	/**
		Create a timer.
	**/
	static public function init(loop:Loop):Timer {
		var timer = new Timer();
		UV.timer_init(loop.uvLoop, timer.uvTimer).resolve();
		return timer;
	}

	/**
		Start the timer.

		`timeout` and `repeat` are in milliseconds.
	**/
	public function start(callback:()->Void, timeout:UInt64, repeat:UInt64) {
		uvTimer.timer_start(Callable.fromStaticFunction(uvTimerCb), timeout, repeat).resolve();
		onTick = callback;
	}

	static function uvTimerCb(uvTimer:Star<UvTimerT>) {
		var timer = Std.downcast(Handle.getHandle(cast uvTimer), Timer);
		timer.onTick();
	}

	/**
		Stop the timer
	**/
	public function stop() {
		UV.timer_stop(uvTimer).resolve();
		onTick = null;
	}
}