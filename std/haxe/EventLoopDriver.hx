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

package haxe;

/**
	Owns waiting for an `EventLoop`. The loop keeps bookkeeping (queues, timers,
	promises); the driver performs `wait` / `wake` / `close`.

	`allowsReentrancy` is read-only-by-contract: implementations expose a fixed
	value and callers must not attempt to mutate it.

	`wake` and `close` must be safe after `close` and from other threads
	(no-op if already closed).
**/
interface EventLoopDriver {
	/**
		When `false`, nested `loopOnce` during native/driver callbacks is forbidden.
		Read-only by contract after construction.
	**/
	final allowsReentrancy:Bool;

	/**
		Block according to `maxBlock` (seconds):

		- `< 0`: do not block; poll only (Haxe work is already due)
		- `0`: block until an event or `wake()`
		- `> 0`: block at most this many seconds

		Callers must never pass the `EventLoop.getNextTick()` idle sentinel `1e6`
		as a real deadline. `EventLoop.loop()` synthesizes `maxBlock` before
		calling `wait` (work due → `-1`; next timer → positive delta; keep-alive
		only → `0`).
	**/
	function wait(maxBlock:Float):Void;

	/**
		Wake a thread blocked in `wait`. No-op if the driver is closed.
		Safe to call from other threads.
	**/
	function wake():Void;

	/**
		Release driver resources. Idempotent; subsequent `wake` / `close` /
		`wait` are no-ops (or `wait` returns immediately). Safe from other threads.
	**/
	function close():Void;

	/**
		Whether the driver has external work that should keep the event loop alive
		(for example native UV handles). The default Haxe driver always returns `false`.
	**/
	function hasExternalWork():Bool;
}
