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

// This file is generated from mozilla\FetchEvent.webidl. Do not edit!

package js.html;

import js.lib.Promise;

/**
	This is the event type for `fetch` events dispatched on the service worker global scope. It contains information about the fetch, including the request and how the receiver will treat the response. It provides the `event.respondWith()` method, which allows us to provide a response to this fetch.

	Documentation [FetchEvent](https://developer.mozilla.org/en-US/docs/Web/API/FetchEvent) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/FetchEvent$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/FetchEvent>
**/
@:native("FetchEvent")
extern class FetchEvent extends ExtendableEvent {

	/**
		The `Request` the browser intends to make.
	**/
	var request(default,null) : Request;

	/**
		The `Client.id` of the same-origin `Client` that initiated the fetch.
	**/
	var clientId(default,null) : String;
	var isReload(default,null) : Bool;

	/** @throws DOMError */
	function new( type : String, eventInitDict : FetchEventInit ) : Void;

	/**
		Prevent the browser's default fetch handling, and provide (a promise for) a response yourself.
		@throws DOMError
	**/
	function respondWith( r : Promise<Response> ) : Void;
}