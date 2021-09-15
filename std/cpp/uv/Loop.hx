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

enum LoopOption {
	/**
		Block a signal when polling for new events.

		This operation is currently only implemented for SIGPROF signals.
		Requesting other signals will fail with `UV_EINVAL`.

		TODO: change Int to `enum abstract SigNum` when `cpp.uv.Signal` is implemented.
	**/
	BlockSignal(sigNum:Int);
	/** Accumulate the amount of idle time the event loop spends in the event provider. */
	MetricsIdleTime;
}

/**
	Mode used to run the loop with `Loop.run`
**/
enum abstract LoopRunMode(Int) {
	/**
		Runs the event loop until there are no more active and referenced handles
		or requests.
		`loop.run(DEFAULT)` returns `true` if `loop.stop()` was called and there
		are still active handles or requests.
	**/
	var DEFAULT;
	/**
		Poll for i/o once. Note that this function blocks if there are no pending
		callbacks.
		`loop.run(ONCE)` returns `false` when done (no active handles or requests
		left), or `true` if more callbacks are expected (meaning you should run
		the event loop again sometime in the future).
	**/
	var ONCE;
	/**
		Poll for i/o once but don’t block if there are no pending callbacks.
		`loop.run(NOWAIT)` returns `false` if done (no active handles or requests
		left), or `true` if more callbacks are expected (meaning you should run
		the event loop again sometime in the future).
	**/
	var NOWAIT;
}

/**
	Event loop.

	@see http://docs.libuv.org/en/v1.x/loop.html
**/
@:headerCode('#include "uv.h"')
class Loop {
	@:allow(cpp.uv)
	var uvLoop:Star<UvLoopT>;

	function new() {
		uvLoop = UvLoopT.create();
		cpp.vm.Gc.setFinalizer(this, Function.fromStaticFunction(finalizer));
	}

	static function finalizer(loop:Loop) {
		Native.free(loop.uvLoop);
	}

	/**
		Creates and initializes a loop.
	**/
	static public function init():Loop {
		var loop = new Loop();
		UV.loop_init(loop.uvLoop).resolve();
		return loop;
	}

	/**
		Set additional loop options.

		You should normally call this before the first call to `loop.run()` unless
		mentioned otherwise.
	**/
	public function configure(option:LoopOption):Void {
		var result = switch option {
			case BlockSignal(sigNum):
				UV.loop_configure(uvLoop, UV_LOOP_BLOCK_SIGNAL, sigNum);
			case MetricsIdleTime:
				UV.loop_configure(uvLoop, UV_METRICS_IDLE_TIME);
		}
		result.resolve();
	}

	/**
		Releases all internal loop resources.

		Call this function only when the loop has finished executing and all open
		handles and requests have been closed.
	**/
	public function close():Void {
		UV.loop_close(uvLoop).resolve();
	}

	/**
		This function runs the event loop.
	**/
	public function run(mode:LoopRunMode = DEFAULT):Bool {
		var result = switch mode {
			case DEFAULT: UV.run(uvLoop, UV_RUN_DEFAULT);
			case ONCE: UV.run(uvLoop, UV_RUN_ONCE);
			case NOWAIT: UV.run(uvLoop, UV_RUN_NOWAIT);
		}
		return 0 != result.resolve();
	}

	/**
		Returns `true` if there are referenced active handles, active requests or
		closing handles in the loop.
	**/
	public function alive():Bool {
		return 0 != UV.loop_alive(uvLoop).resolve();
	}

	/**
		Stop the event loop, causing `loop.run()` to end as soon as possible.
	**/
	public function stop() {
		return UV.stop(uvLoop);
	}

	/**
		Get backend file descriptor.

		Only kqueue, epoll and event ports are supported.
	**/
	public function backendFd():Int {
		return UV.backend_fd(uvLoop);
	}

	/**
		Get the poll timeout.

		The return value is in milliseconds, or -1 for no timeout.
	**/
	public function backendTimeout():Int {
		return UV.backend_timeout(uvLoop);
	}

	/**
		Return the current timestamp in milliseconds.
	**/
	public function now():UInt64 {
		return UV.now(uvLoop);
	}

	/**
		Update the event loop’s concept of “now”.
	**/
	public function updateTime() {
		UV.update_time(uvLoop);
	}
}