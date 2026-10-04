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

enum abstract LoopRunMode(Int) {
	var Default = 0;
	var Once = 1;
	var NoWait = 2;
}

abstract Loop(hl.Abstract<"uv_loop">) {
	@:hlNative("uv", #if (hl_ver >= version("1.16.0")) "loop_close_wrap" #else "loop_close" #end)
	public function close():Int {
		return 0;
	}

	@:hlNative("uv", #if (hl_ver >= version("1.16.0")) "run_wrap" #else "run" #end)
	public function run(mode:LoopRunMode):Int {
		return 0;
	}

	@:hlNative("uv", #if (hl_ver >= version("1.16.0")) "loop_alive_wrap" #else "loop_alive" #end)
	public function alive():Int {
		return 0;
	}

	@:hlNative("uv", #if (hl_ver >= version("1.16.0")) "stop_wrap" #else "stop" #end)
	public function stop():Void {}

	/**
		Attach a libuv loop to `loop` as its waiting driver and return the UV
		`Loop` **synchronously**.

		Install uses `swapDriver`: when called from an event callback (`inLoop`)
		or another thread, apply is deferred until the next `applyPendingSwap`.
		A pending UV swap counts as external work so `loop()` cannot idle-exit
		before the driver is published. Idempotent: if a UV driver is already
		current or pending for this EventLoop, its loop is reused.
	**/
	public static function getFromEventLoop(loop:haxe.EventLoop):Loop {
		final current = loop.getDriver();
		if (Std.isOfType(current, UvEventLoopDriver))
			return (cast current : UvEventLoopDriver).uvLoop;
		final pending = loop.getPendingDriver();
		if (pending != null && Std.isOfType(pending, UvEventLoopDriver))
			return (cast pending : UvEventLoopDriver).uvLoop;

		final isDefault = loop == haxe.EventLoop.main;
		final uvLoop = if (isDefault) {
			default_loop();
		} else {
			#if (hl_ver < version("1.16.0"))
			throw "Using libUV multithread requires -D hl-ver=1.16.0";
			#else
			create();
			#end
		};
		final driver = new UvEventLoopDriver(uvLoop, isDefault);
		// Often deferred when called inside a callback; sync only when rule #5 allows.
		loop.swapDriver(driver);
		return uvLoop;
	}

	public static function getCurrent():Loop {
		return getFromEventLoop(haxe.EventLoop.current);
	}

	public static function getDefault():Loop {
		return getFromEventLoop(haxe.EventLoop.main);
	}

	@:hlNative("uv", #if (hl_ver >= version("1.16.0")) "default_loop_wrap" #else "default_loop" #end)
	static function default_loop():Loop {
		return null;
	}

	#if (hl_ver >= version("1.16.0"))
	@:hlNative("uv", "create_loop") public static function create():Loop {
		return null;
	}
	#end

}
