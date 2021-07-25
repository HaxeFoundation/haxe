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

/**
	Timers.

	@see http://docs.libuv.org/en/v1.x/timer.html
**/
@:forward
abstract Timer(Handle) to Handle {
	/** The timer repeat value. */
	public var repeat(get,set):Int;
	@:hlNative("uv", "timer_get_repeat_wrap") function get_repeat():Int return 0;
	@:hlNative("uv", "timer_set_repeat_wrap") function set_repeat(v:Int):Int return v;

	/** Get the timer due value or 0 if it has expired. */
	public var dueIn(get,never):Int;
	@:hlNative("uv", "timer_get_due_in_wrap") public function get_dueIn():Int return 0;

	/**
		Initialize the timer.
	**/
	@:hlNative("uv", "timer_init_wrap")
	static public function init(loop:Loop):Timer
		return null;

	/**
		Start the timer.
		`timeout` and `repeat` are in milliseconds.

		If `timeout` is zero, the `callback` fires on the next event loop iteration.
		If `repeat` is non-zero, the `callback` fires first after `timeout` milliseconds
		and then repeatedly after `repeat` milliseconds.
	**/
	@:hlNative("uv", "timer_start_wrap")
	public function start(callback:()->Void, timeout:Int, repeat:Int):Void {}

	/**
		Stop the timer, the callback will not be called anymore.
	**/
	@:hlNative("uv", "timer_stop_wrap")
	public function stop():Void {}

	/**
		Stop the timer, and if it is repeating restart it using the repeat value
		as the timeout.
		If the timer has never been started before it throws UV_EINVAL.
	**/
	@:hlNative("uv", "timer_again_wrap")
	public function again():Void {}
}