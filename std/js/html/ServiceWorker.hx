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

// This file is generated from mozilla\ServiceWorker.webidl. Do not edit!

package js.html;

/**
	The `ServiceWorker` interface of the ServiceWorker API provides a reference to a service worker. Multiple browsing contexts (e.g. pages, workers, etc.) can be associated with the same service worker, each through a unique `ServiceWorker` object.

	Documentation [ServiceWorker](https://developer.mozilla.org/en-US/docs/Web/API/ServiceWorker) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/ServiceWorker$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/ServiceWorker>
**/
@:native("ServiceWorker")
extern class ServiceWorker extends EventTarget
{
	
	/**
		Returns the `ServiceWorker` serialized script URL defined as part of `ServiceWorkerRegistration`. The URL must be on the same origin as the document that registers the `ServiceWorker`.
	**/
	var scriptURL(default,null) : String;
	
	/**
		Returns the state of the service worker. It returns one of the following values: `installing`, `installed,` `activating`, `activated`, or `redundant`.
	**/
	var state(default,null) : ServiceWorkerState;
	
	/**
		An `EventListener` property called whenever an event of type `statechange` is fired; it is basically fired anytime the `ServiceWorker.state` changes.
	**/
	var onstatechange : haxe.Constraints.Function;
	var onerror : haxe.Constraints.Function;
	
	/** @throws DOMError */
	function postMessage( message : Dynamic, ?transferable : Array<Dynamic> ) : Void;
}