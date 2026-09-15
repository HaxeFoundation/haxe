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

	public static function getFromEventLoop(loop:haxe.EventLoop):Loop {
		if (@:privateAccess loop.nativeLoop == null) {
			if (loop == haxe.EventLoop.main)
				@:privateAccess loop.nativeLoop = new LoopWrapper(default_loop());
			else {
				#if (hl_ver < version("1.16.0"))
				throw "Using libUV multithread requires -D hl-ver=1.16.0";
				#else
				@:privateAccess loop.nativeLoop = new LoopWrapper(create());
				#end
			}
		}
		final wrapped:LoopWrapper = cast @:privateAccess loop.nativeLoop;
		return @:privateAccess wrapped.uvLoop;
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

/**
	NativeEventLoop adapter: blocking `UV_RUN_ONCE` with an async wake doorbell
	and a one-shot UV timer for the next Haxe EventLoop deadline.
**/
private class LoopWrapper {
	public final allowsReentrancy = false;
	final uvLoop:Loop;
	final keepAliveCb:Void->Void;
	var asyncHandle:HandleData;
	var timerHandle:HandleData;
	var closed = false;

	public function new(loop:Loop) {
		this.uvLoop = loop;
		// Closures stored in uv handle data are in hl_gc_alloc_raw; keep a Haxe reference.
		keepAliveCb = function() {};
		asyncHandle = async_init(loop, keepAliveCb);
		if (asyncHandle == null)
			throw "Failed to create uv_async_t wake handle";
		handle_unref(asyncHandle);
		timerHandle = timer_init(loop);
		if (timerHandle == null)
			throw "Failed to create uv_timer_t deadline handle";
		handle_unref(timerHandle);
	}

	public function run(maxBlock:Float) {
		if (closed)
			return;
		if (maxBlock < 0) {
			// Haxe events already due: do not sleep in the poller
			stopDeadlineTimer();
			uvLoop.run(NoWait);
			return;
		}
		if (maxBlock > 0)
			armDeadlineTimer(maxBlock);
		else
			stopDeadlineTimer();
		uvLoop.run(Once);
		stopDeadlineTimer();
	}

	public function wake() {
		if (asyncHandle != null)
			async_send(asyncHandle);
	}

	public function close() {
		if (closed)
			return;
		closed = true;
		stopDeadlineTimer();
		if (asyncHandle != null) {
			close_handle(asyncHandle, null);
			asyncHandle = null;
		}
		if (timerHandle != null) {
			close_handle(timerHandle, null);
			timerHandle = null;
		}
		// Drain close callbacks so loop_close can succeed
		uvLoop.run(NoWait);
		final result = uvLoop.close();
		if (result != 0)
			Sys.println("Some async handlers have not been closed");
	}

	public function isAlive() {
		return !closed && uvLoop.alive() > 0;
	}

	function armDeadlineTimer(maxBlock:Float) {
		if (timerHandle == null)
			return;
		var ms = Math.ceil(maxBlock * 1000);
		if (ms < 1)
			ms = 1;
		if (ms > 2147483647)
			ms = 2147483647;
		timer_start(timerHandle, keepAliveCb, ms, 0);
	}

	function stopDeadlineTimer() {
		if (timerHandle != null)
			timer_stop(timerHandle);
	}

	@:hlNative("uv", "async_init_wrap")
	static function async_init(loop:Loop, callb:Void->Void):HandleData {
		return null;
	}

	@:hlNative("uv", "async_send_wrap")
	static function async_send(h:HandleData):Void {}

	@:hlNative("uv", "timer_init_wrap")
	static function timer_init(loop:Loop):HandleData {
		return null;
	}

	@:hlNative("uv", "timer_start_wrap")
	static function timer_start(h:HandleData, callb:Void->Void, timeout:Int, repeat:Int):Bool {
		return false;
	}

	@:hlNative("uv", "timer_stop_wrap")
	static function timer_stop(h:HandleData):Bool {
		return false;
	}

	@:hlNative("uv", "handle_unref_wrap")
	static function handle_unref(h:HandleData):Void {}

	@:hlNative("uv", "close_handle")
	static function close_handle(h:HandleData, callb:Null<Void->Void>):Void {}
}
