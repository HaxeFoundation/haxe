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

// This file is generated from mozilla\ServiceWorkerRegistration.webidl. Do not edit!

package js.html;

/**
	The `ServiceWorkerRegistration` interface of the ServiceWorker API represents the service worker registration. You register a service worker to control one or more pages that share the same origin.

	Documentation [ServiceWorkerRegistration](https://developer.mozilla.org/en-US/docs/Web/API/ServiceWorkerRegistration) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/ServiceWorkerRegistration$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/ServiceWorkerRegistration>
**/
@:native("ServiceWorkerRegistration")
extern class ServiceWorkerRegistration extends EventTarget
{
	
	/**
		Returns a service worker whose state is `installing`. This is initially set to `null`.
	**/
	var installing(default,null) : ServiceWorker;
	
	/**
		Returns a service worker whose state is `installed`. This is initially set to `null`.
	**/
	var waiting(default,null) : ServiceWorker;
	
	/**
		Returns a service worker whose state is either `activating` or `activated`. This is initially set to `null`. An active worker will control a `ServiceWorkerClient` if the client's URL falls within the scope of the registration (the `scope` option set when `ServiceWorkerContainer.register` is first called.)
	**/
	var active(default,null) : ServiceWorker;
	
	/**
		Returns a unique identifier for a service worker registration. This must be on the same origin as the document that registers the `ServiceWorker`.
	**/
	var scope(default,null) : String;
	
	/**
		An `EventListener` property called whenever an event of type `updatefound` is fired; it is fired any time the `ServiceWorkerRegistration.installing` property acquires a new service worker.
	**/
	var onupdatefound : haxe.Constraints.Function;
	
	/** @throws DOMError */
	
	/**
		Checks the server for an updated version of the service worker without consulting caches.
	**/
	function update() : Promise<Void>;
	/** @throws DOMError */
	
	/**
		Unregisters the service worker registration and returns a promise (see `Promise`). The service worker will finish any ongoing operations before it is unregistered.
	**/
	function unregister() : Promise<Bool>;
}