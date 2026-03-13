/*
 * Copyright (C)2014-2020 Haxe Foundation
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

package js.node;

import haxe.Constraints.Function;
import js.node.events.EventEmitter;
#if haxe4
import js.lib.Promise;
#else
import js.Promise;
#end

/**
	Much of the Node.js core API is built around an idiomatic asynchronous event-driven architecture
	in which certain kinds of objects (called "emitters") emit named events that cause `Function` objects
	("listeners") to be called.

	@see https://nodejs.org/api/events.html#events_events
 */
@:jsRequire("events")
extern class Events {
	/**
		Creates a `Promise` that is resolved when the `EventEmitter` emits the given
		event or that is rejected when the `EventEmitter` emits `'error'`.
		The `Promise` will resolve with an array of all the arguments emitted to the
		given event.

		@see https://nodejs.org/api/events.html#events_events_once_emitter_name
	**/
	static function once<T:Function>(emitter:IEventEmitter, name:Event<T>):Promise<Array<Dynamic>>;
}
