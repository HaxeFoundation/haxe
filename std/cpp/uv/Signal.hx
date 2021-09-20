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

@:using(cpp.uv.Signal)
@:headerCode('#include "uv.h"')
enum abstract SigNum(Int) from Int {
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
			case _:
				switch (this:NativeSigNum) {
					case NativeSigNum.SIGABRT: 'SIGABRT';
					case NativeSigNum.SIGFPE: 'SIGFPE';
					case NativeSigNum.SIGHUP: 'SIGHUP';
					case NativeSigNum.SIGILL: 'SIGILL';
					case NativeSigNum.SIGINT: 'SIGINT';
					case NativeSigNum.SIGKILL: 'SIGKILL';
					case NativeSigNum.SIGSEGV: 'SIGSEGV';
					case NativeSigNum.SIGTERM: 'SIGTERM';
					case NativeSigNum.SIGWINCH: 'SIGWINCH';
					case _: 'SIG#$this';
				}
		}
	}
}

/**
	Signal handles implement Unix style signal handling on a per-event loop bases.

	@see http://docs.libuv.org/en/v1.x/signal.html
**/
@:headerCode('#include "uv.h"')
class Signal extends Handle {
	var uvSignal:RawPointer<UvSignalT>;
	var onSignal:()->Void;
	var onSignalOnce:()->Void;

	function setupUvHandle() {
		uvSignal = UvSignalT.create();
		uvHandle = cast uvSignal;
	}

	/**
		Convert `SigNum` value to a corresponding signal number.
	**/
	static public function toInt(sigNum:SigNum):Int {
		return switch sigNum {
			case SIGABRT: NativeSigNum.SIGABRT;
			case SIGFPE: NativeSigNum.SIGFPE;
			case SIGHUP: NativeSigNum.SIGHUP;
			case SIGILL: NativeSigNum.SIGILL;
			case SIGINT: NativeSigNum.SIGINT;
			case SIGKILL: NativeSigNum.SIGKILL;
			case SIGSEGV: NativeSigNum.SIGSEGV;
			case SIGTERM: NativeSigNum.SIGTERM;
			case SIGWINCH: NativeSigNum.SIGWINCH;
			case _: cast sigNum;
		}
	}

	/**
		Convert integer signal number to a corresponding `SigNum` value.
	**/
	static public function fromInt(sigNum:Int):SigNum {
		return switch sigNum {
			case NativeSigNum.SIGABRT: SIGABRT;
			case NativeSigNum.SIGFPE: SIGFPE;
			case NativeSigNum.SIGHUP: SIGHUP;
			case NativeSigNum.SIGILL: SIGILL;
			case NativeSigNum.SIGINT: SIGINT;
			case NativeSigNum.SIGKILL: SIGKILL;
			case NativeSigNum.SIGSEGV: SIGSEGV;
			case NativeSigNum.SIGTERM: SIGTERM;
			case NativeSigNum.SIGWINCH: SIGWINCH;
			case _: sigNum;
		}
	}

	/**
		Create a signal.
	**/
	static public function init(loop:Loop):Signal {
		var signal = new Signal();
		UV.signal_init(loop.uvLoop, signal.uvSignal).resolve();
		return signal;
	}

	/**
		Start the handle with the given callback.
	**/
	public function start(sigNum:SigNum, callback:()->Void) {
		uvSignal.signal_start(Callable.fromStaticFunction(uvSignalCb), sigNum.toInt()).resolve();
		onSignal = callback;
	}

	/**
		Like `cpp.uv.Signal.start`, but the handle is stopped after one callback call.
	**/
	public function startOneshot(sigNum:SigNum, callback:()->Void) {
		uvSignal.signal_start_oneshot(Callable.fromStaticFunction(uvSignalCbOnce), sigNum.toInt()).resolve();
		onSignal = callback;
	}

	static function uvSignalCb(uvSignal:RawPointer<UvSignalT>, sigNum:Int) {
		var signal:Signal = cast Handle.getHandle(cast uvSignal);
		signal.onSignal();
	}

	static function uvSignalCbOnce(uvSignal:RawPointer<UvSignalT>, sigNum:Int) {
		var signal:Signal = cast Handle.getHandle(cast uvSignal);
		var cb = signal.onSignalOnce;
		signal.onSignalOnce = null;
		cb();
	}

	/**
		Stop the signal.
	**/
	public function stop() {
		UV.signal_stop(uvSignal).resolve();
		onSignal = null;
	}
}