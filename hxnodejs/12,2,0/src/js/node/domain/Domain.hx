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

package js.node.domain;

import haxe.Constraints.Function;
import js.node.Timers.Timeout;
import js.node.events.EventEmitter;

/**
	Enumeration of events emitted by `Domain` objects.
**/
@:deprecated
enum abstract DomainEvent<T:Function>(Event<T>) to Event<T> {
	var Error:DomainEvent<DomainError->Void> = "error";
	var Dispose:DomainEvent<Void->Void> = "dispose";
}

/**
	Any time an Error object is routed through a domain, a few extra fields are added to it.
**/
@:deprecated
typedef DomainError = {
	/**
		The domain that first handled the error.
	**/
	var domain:Domain;

	/**
		The event emitter that emitted an 'error' event with the error object.
	**/
	var domainEmitter:IEventEmitter;

	/**
		The callback function which was bound to the domain, and passed an error as its first argument.
	**/
	var domainBound:Function;

	/**
		A boolean indicating whether the error was thrown, emitted, or passed to a bound callback function.
	**/
	var domainThrown:Bool;
}

/**
	The Domain class encapsulates the functionality of routing errors
	and uncaught exceptions to the active Domain object.
**/
@:deprecated
extern class Domain extends EventEmitter<Domain> {
	/**
		Run the supplied function in the context of the domain, implicitly binding all event emitters, timers,
		and lowlevel requests that are created in that context.

		This is the most basic way to use a domain.
	**/
	function run(fn:Void->Void):Void;

	/**
		An array of timers and event emitters that have been explicitly added to the domain.
	**/
	var members(default, null):Array<haxe.extern.EitherType<IEventEmitter, Timeout>>;

	/**
		Explicitly adds an `emitter` to the domain.

		If any event handlers called by the emitter throw an error, or if the emitter emits an error event,
		it will be routed to the domain's error event, just like with implicit binding.

		This also works with timers that are returned from `setInterval` and `setTimeout`.
		If their callback function throws, it will be caught by the domain 'error' handler.

		If the Timer or EventEmitter was already bound to a domain, it is removed from that one,
		and bound to this one instead.
	**/
	@:overload(function(emitter:Timeout):Void {})
	function add(emitter:IEventEmitter):Void;

	/**
		The opposite of `add`. Removes domain handling from the specified emitter.
	**/
	@:overload(function(emitter:Timeout):Void {})
	function remove(emitter:IEventEmitter):Void;

	/**
		The returned function will be a wrapper around the supplied `callback` function.
		When the returned function is called, any errors that are thrown will be routed to the domain's error event.
	**/
	function bind<T:Function>(callback:T):T;

	/**
		This method is almost identical to `bind`. However, in addition to catching thrown errors, it will also
		intercept `Error` objects sent as the first argument to the function.

		In this way, the common if (er != null) return callback(er); pattern
		can be replaced with a single error handler in a single place.
	**/
	function intercept<T:Function>(callback:T):T;

	/**
		The `enter` method is plumbing used by the `run`, `bind`, and `intercept` methods to set the active domain.

		It sets `domain.active` and `process.domain` to the domain, and implicitly pushes the domain onto
		the domain stack managed by the domain module (see `exit` for details on the domain stack).

		The call to `enter` delimits the beginning of a chain of asynchronous calls and I/O operations bound to a domain.

		Calling `enter` changes only the active domain, and does not alter the domain itself.
		Enter and exit can be called an arbitrary number of times on a single domain.

		If the domain on which `enter` is called has been disposed, `enter` will return without setting the domain.
	**/
	function enter():Void;

	/**
		The `exit` method exits the current domain, popping it off the domain stack.

		Any time execution is going to switch to the context of a different chain of asynchronous calls,
		it's important to ensure that the current domain is exited. The call to `exit` delimits either the end of
		or an interruption to the chain of asynchronous calls and I/O operations bound to a domain.

		If there are multiple, nested domains bound to the current execution context,
		`exit` will exit any domains nested within this domain.

		Calling `exit` changes only the active domain, and does not alter the domain itself.
		Enter and exit can be called an arbitrary number of times on a single domain.

		If the domain on which `exit` is called has been disposed, `exit` will return without exiting the domain.
	**/
	function exit():Void;

	/**
		The `dispose` method destroys a domain, and makes a best effort attempt
		to clean up any and all IO that is associated with the domain.

		Streams are aborted, ended, closed, and/or destroyed. Timers are cleared.
		Explicitly bound callbacks are no longer called.

		Any error events that are raised as a result of this are ignored.

		The intention of calling `dispose` is generally to prevent cascading errors when a critical part of
		the Domain context is found to be in an error state.

		Once the domain is disposed the 'dispose' event will emit.

		Note that IO might still be performed. However, to the highest degree possible, once a domain is disposed,
		further errors from the emitters in that set will be ignored. So, even if some remaining actions are still
		in flight, Node.js will not communicate further about them.
	**/
	function dispose():Void;
}
