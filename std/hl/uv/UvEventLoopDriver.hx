/*
 * Copyright (C)2005-2026 Haxe Foundation
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
	LibUV-backed `haxe.EventLoopDriver`.

	Owns an async doorbell and a one-shot deadline timer on `uvLoop`. While
	`wait(maxBlock)` blocks (`maxBlock >= 0`), the async handle is referenced so
	`UV_RUN_ONCE` does not busy-spin when only driver handles exist. Outside
	waits both handles stay unreferenced so they alone do not keep the loop
	alive (`hasExternalWork`).

	`isDefault` must be `true` for the process-global `default_loop`: `close`
	then only closes driver-owned handles and never calls `uv_loop_close`.
**/
class UvEventLoopDriver implements haxe.EventLoopDriver {
	public final allowsReentrancy = false;

	/** Underlying libuv loop (for `getFromEventLoop` idempotent reuse). **/
	public final uvLoop:Loop;

	final isDefault:Bool;
	/** Closures stored in uv handle data are in hl_gc_alloc_raw; keep a Haxe ref. **/
	final keepAliveCb:Void->Void;
	var asyncHandle:HandleData;
	var timerHandle:HandleData;
	var closed = false;

	/**
		@param uvLoop libuv loop to drive
		@param isDefault `true` when `uvLoop` is the process default loop
	**/
	public function new(uvLoop:Loop, isDefault:Bool) {
		this.uvLoop = uvLoop;
		this.isDefault = isDefault;
		keepAliveCb = function() {};
		asyncHandle = async_init(uvLoop, keepAliveCb);
		if (asyncHandle == null)
			throw "Failed to create uv_async_t wake handle";
		handle_unref(asyncHandle);
		timerHandle = timer_init(uvLoop);
		if (timerHandle == null)
			throw "Failed to create uv_timer_t deadline handle";
		handle_unref(timerHandle);
	}

	public function wait(maxBlock:Float):Void {
		if (closed)
			return;
		if (maxBlock < 0) {
			stopDeadlineTimer();
			uvLoop.run(NoWait);
			return;
		}
		if (maxBlock > 0)
			armDeadlineTimer(maxBlock);
		else
			stopDeadlineTimer();
		// Ref async for the blocking poll so wait(0)/wait(t) cannot busy-spin
		// when only unref'd driver handles exist.
		final async = asyncHandle;
		if (async != null)
			handle_ref(async);
		uvLoop.run(Once);
		if (async != null)
			handle_unref(async);
		stopDeadlineTimer();
	}

	public function wake():Void {
		if (closed)
			return;
		final async = asyncHandle;
		if (async != null)
			async_send(async);
	}

	public function close():Void {
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
		// Drain close callbacks
		uvLoop.run(NoWait);
		if (!isDefault) {
			final result = uvLoop.close();
			if (result != 0)
				Sys.println("Some async handlers have not been closed");
		}
	}

	public function hasExternalWork():Bool {
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

	@:hlNative("uv", "handle_ref_wrap")
	static function handle_ref(h:HandleData):Void {}

	@:hlNative("uv", "handle_unref_wrap")
	static function handle_unref(h:HandleData):Void {}

	@:hlNative("uv", "close_handle")
	static function close_handle(h:HandleData, callb:Null<Void->Void>):Void {}
}
