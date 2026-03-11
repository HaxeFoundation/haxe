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

package haxe;

/**
	A GC finalizer registry that invokes a callback when a watched object
	is garbage-collected. Modeled after JavaScript's `FinalizationRegistry`.

	The callback receives a held value (not the collected object itself),
	which is safe because the object may already be in an invalid state.

	Not all targets support GC finalizers. On unsupported targets, the
	constructor throws `NotImplementedException`.
**/
class GcFinalizer<T> {
	/**
		Creates a new `GcFinalizer` with the given cleanup `callback`.
		The callback will be invoked with the held value when a registered
		target object is garbage-collected.
	**/
	public function new(callback:T->Void) {
		throw new haxe.exceptions.NotImplementedException("Not implemented for this platform");
	}

	/**
		Registers `target` for clean-up. When `target` is garbage-collected,
		the callback will be invoked with `heldValue`.

		Returns an `IHandle` handle. Calling `close()` on the handle
		cancels the registration, preventing the callback from firing.
	**/
	public function register(target:{}, heldValue:T):IHandle {
		return null;
	}
}
