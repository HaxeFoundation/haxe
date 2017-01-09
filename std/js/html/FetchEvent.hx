/*
 * Copyright (C)2005-2017 Haxe Foundation
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

/**
	The parameter passed into the `ServiceWorkerGlobalScope.onfetch` handler, `FetchEvent` represents a fetch action that is dispatched on the `ServiceWorkerGlobalScope` of a `ServiceWorker`. It contains information about the request and resulting response, and provides the `FetchEvent.respondWith()` method, which allows us to provide an arbitrary response back to the controlled page.

	Documentation [FetchEvent](https://developer.mozilla.org/en-US/docs/Web/API/FetchEvent) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/FetchEvent$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/FetchEvent>
**/
@:native("FetchEvent")
extern class FetchEvent extends ExtendableEvent
{
	
	/**
		Returns the `Request` that triggered the event handler.
	**/
	var request(default,null) : Request;
	
	/**
		Returns the id of the client that the current service worker is controlling.
	**/
	var clientId(default,null) : String;
	
	/**
		Returns a `Boolean` that is `true` if the event was dispatched with the user's intention for the page to reload, and `false` otherwise. Typically, pressing the refresh button in a browser is a reload, while clicking a link and pressing the back button is not.
	**/
	var isReload(default,null) : Bool;
	
	/** @throws DOMError */
	function new( type : String, eventInitDict : FetchEventInit ) : Void;
	/** @throws DOMError */
	
	/**
		Resolves by returning a `Response` or a network error  to Fetch`.
	**/
	function respondWith( r : Promise<Response> ) : Void;
}