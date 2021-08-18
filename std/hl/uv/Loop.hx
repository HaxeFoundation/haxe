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
	Run modes for `hl.uv.Loop.run(mode)`
**/
enum abstract LoopRunMode(Int) {
	/** Runs the event loop until there are no more active and referenced handles or requests. */
	var Default = 0;
	/** Poll for i/o once. Note that this function blocks if there are no pending callbacks. */
	var Once = 1;
	/** Poll for i/o once but don’t block if there are no pending callbacks. */
	var NoWait = 2;
}

/**
	Event loops.

	@see http://docs.libuv.org/en/v1.x/loop.html

	Haxe event loops define an implicit cast to libuv loops. That is, you can use
	`sys.thread.Thread.current().events` in any place where `eval.luv.Loop` is
	expected.
**/
abstract Loop(hl.Abstract<"uv_loop">) {
	var loop(get,never):Loop;
	inline function get_loop():Loop return cast this;

	@:from
	static inline function fromHaxeEventLoop(events:sys.thread.EventLoop):Loop
		return events.handle;

	/**
		Allocate and initialize an event loop.
	**/
	static public function init():Loop {
		var loop = UV.alloc_loop();
		loop.loop_init().resolve();
		return loop;
	}

	/**
		Releases all internal loop resources.
		Call this function only when the loop has finished executing and all open
		handles and requests have been closed, or it will throw `EBUSY`.
	**/
	public function close():Void {
		loop.loop_close().resolve();
	}

	/**
		This function runs the event loop.

		@see http://docs.libuv.org/en/v1.x/loop.html#c.uv_run
	**/
	public function run(mode:LoopRunMode):Bool {
		return UV.run(loop, mode).resolve() != 0;
	}

	/**
		Returns non-zero if there are referenced active handles, active requests
		or closing handles in the loop.
	**/
	public function alive():Bool {
		return loop.loop_alive() != 0;
	}

	/**
		Stop the event loop as soon as possible.
	**/
	public function stop():Void {
		UV.stop(loop);
	}

	/**
		Returns the initialized default loop.

		@see http://docs.libuv.org/en/v1.x/loop.html#c.uv_default_loop
	**/
	public static function getDefault():Loop {
		var def = UV.default_loop();
		if (loopEvent == null)
			loopEvent = haxe.MainLoop.add(function() {
				// if no more things to process, stop
				if (!def.run(NoWait)) {
					loopEvent.stop();
					loopEvent = null;
				}
			});
		return def;
	}

	static var loopEvent:haxe.MainLoop.MainEvent;
}
