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

// This file is generated from mozilla\ServiceWorkerContainer.webidl. Do not edit!

package js.html;

import js.lib.Promise;

/**
	The `ServiceWorkerContainer` interface of the ServiceWorker API provides an object representing the service worker as an overall unit in the network ecosystem, including facilities to register, unregister and update service workers, and access the state of service workers and their registrations.

	Documentation [ServiceWorkerContainer](https://developer.mozilla.org/en-US/docs/Web/API/ServiceWorkerContainer) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/ServiceWorkerContainer$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/ServiceWorkerContainer>
**/
@:native("ServiceWorkerContainer")
extern class ServiceWorkerContainer extends EventTarget {

	/**
		Returns a `ServiceWorker` object if its state is `activated` (the same object returned by `ServiceWorkerRegistration.active`). This property returns `null` during a force-refresh request (Shift + refresh) or if there is no active worker.
	**/
	var controller(default,null) : ServiceWorker;

	/**
		Provides a way of delaying code execution until a service worker is active. It returns a `Promise` that will never reject, and which waits indefinitely until the `ServiceWorkerRegistration` associated with the current page has an `ServiceWorkerRegistration.active` worker. Once that condition is met, it resolves with the `ServiceWorkerRegistration`.
	**/
	var ready(default,null) : Promise<ServiceWorkerRegistration>;

	/**
		Fired whenever a `controllerchange` event occurs — when the document's associated `ServiceWorkerRegistration` acquires a new `ServiceWorkerRegistration.active` worker.
	**/
	var oncontrollerchange : haxe.Constraints.Function;

	/**
		Fired whenever an `error` event occurs in the associated service workers.
	**/
	var onerror : haxe.Constraints.Function;

	/**
		Fired whenever a `message` event occurs — when incoming messages are received to the `ServiceWorkerContainer` object (e.g. via a `MessagePort.postMessage()` call.)
	**/
	var onmessage : haxe.Constraints.Function;

	function register( scriptURL : String, ?options : RegistrationOptions ) : Promise<ServiceWorkerRegistration>;

	/**
		Gets a `ServiceWorkerRegistration` object whose scope matches the provided document URL.  If the method can't return a `ServiceWorkerRegistration`, it returns a `Promise`. 
	**/
	function getRegistration( documentURL : String = "" ) : Promise<Dynamic>;

	/**
		Returns all `ServiceWorkerRegistration` objects associated with a `ServiceWorkerContainer` in an array.  If the method can't return `ServiceWorkerRegistration` objects, it returns a `Promise`. 
	**/
	function getRegistrations() : Promise<Array<ServiceWorkerRegistration>>;
}