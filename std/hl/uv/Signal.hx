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
	Predefined signal numbers.
	`hl.uv.Signal` methods also accept native signal numbers instead of these constants.

	TODO:
	Find how to set these constants to native signal numbers.
**/
enum abstract SigNum(Int) from Int to Int {
	var SIGABRT = -1;
	var SIGFPE = -2;
	var SIGHUP = -3;
	var SIGILL = -4;
	var SIGINT = -5;
	var SIGKILL = -6;
	var SIGSEGV = -7;
	var SIGTERM = -8;
	var SIGWINCH = -9;

	public function toString():String {
		return switch (this:SigNum) {
			case SIGABRT: 'SIGABRT';
			case SIGFPE: 'SIGFPE';
			case SIGHUP: 'SIGHUP';
			case SIGILL: 'SIGILL';
			case SIGINT: 'SIGINT';
			case SIGKILL: 'SIGKILL';
			case SIGSEGV: 'SIGSEGV';
			case SIGTERM: 'SIGTERM';
			case SIGWINCH: 'SIGWINCH';
			case _: '#$this';
		}
	}
}

/**
	Signal handles implement Unix style signal handling on a per-event loop bases.

	@see http://docs.libuv.org/en/v1.x/signal.html
**/
@:forward
abstract Signal(Handle) to Handle {

	/** Signal being monitored by this handle. */
	public var sigNum(get,never):SigNum;
	@:hlNative("uv", "signal_get_sigNum_wrap") public function get_sigNum():SigNum return 0;

	/**
		Allocate and initialize the handle.
	**/
	@:hlNative("uv", "signal_init_wrap")
	static public function init(loop:Loop):Signal
		return null;

	/**
		Start the handle with the given callback, watching for the given signal.
	**/
	@:hlNative("uv", "signal_start_wrap")
	public function start(sigNum:SigNum, callback:(sigNum:SigNum)->Void):Void {}

	/**
		Start the handle with the given callback, watching for the given signal.
		The signal handler is reset the moment the signal is received.
	**/
	@:hlNative("uv", "signal_start_oneshot_wrap")
	public function startOneshot(sigNum:SigNum, callback:(sigNum:SigNum)->Void):Void {}

	/**
		Stop the handle, the callback will no longer be called.
	**/
	@:hlNative("uv", "signal_stop_wrap") public function stop():Void {}
}