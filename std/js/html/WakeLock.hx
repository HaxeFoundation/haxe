/*
 * Copyright (C)2005-2023 Haxe Foundation
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

package js.html;

import js.lib.Promise;

/**
	Prevents device screens from dimming or locking when an application needs to keep running.

	Documentation [WakeLock](https://developer.mozilla.org/en-US/docs/Web/API/WakeLock) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/WakeLock/contributors.txt), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/WakeLock>
**/
@:native("WakeLock")
extern class WakeLock {

	/**
		Returns a `Promise` that resolves with a `WakeLockSentinel` object, which allows control over screen dimming and locking.
	**/
	function request(?type: WakeLockType): Promise<WakeLockSentinel>;
}

/**
	Provides a handle to the underlying platform wake lock and can be manually released and reacquired.

	Documentation [WakeLockSentinel](https://developer.mozilla.org/en-US/docs/Web/API/WakeLockSentinel) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/WakeLockSentinel/contributors.txt), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/WakeLockSentinel>
**/
@:native("WakeLockSentinel")
extern class WakeLockSentinel  extends EventTarget {

	/**
		A boolean a that indicates whether a `WakeLockSentinel` has been released yet.
	**/
	final released: Bool;

	/**
		A string representation of the currently acquired `WakeLockSentinel` type.
	**/
	final type: WakeLockType;

	/**
		Releases the `WakeLockSentinel`, returning a `Promise` that is resolved once the sentinel has been successfully released.
	**/
	function release(): Promise<Void>;
}

/**
	Indicates the type of `WakeLockSentinel` to be acquired.
**/
enum abstract WakeLockType(String) from String to String {
	var Screen = "screen";
}
