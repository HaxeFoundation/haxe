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

// This file is generated from mozilla\ServiceWorkerGlobalScope.webidl. Do not edit!

package js.html;

/**
	The `ServiceWorkerGlobalScope` interface of the ServiceWorker API represents the global execution context of a service worker.

	Documentation [ServiceWorkerGlobalScope](https://developer.mozilla.org/en-US/docs/Web/API/ServiceWorkerGlobalScope) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/ServiceWorkerGlobalScope$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/ServiceWorkerGlobalScope>
**/
@:native("ServiceWorkerGlobalScope")
extern class ServiceWorkerGlobalScope extends WorkerGlobalScope
{
	
	/**
		Contains the `Clients` object associated with the service worker.
	**/
	var clients(default,null) : Clients;
	
	/**
		Contains the `ServiceWorkerRegistration` object that represents the service worker's registration.
	**/
	var registration(default,null) : ServiceWorkerRegistration;
	
	/**
		An event handler fired whenever an `install` event occurs — when a `ServiceWorkerRegistration` acquires a new `ServiceWorkerRegistration.installing` worker.
	**/
	var oninstall : haxe.Constraints.Function;
	
	/**
		An event handler fired whenever an `activate` event occurs — when a `ServiceWorkerRegistration` acquires a new `ServiceWorkerRegistration.active` worker.
	**/
	var onactivate : haxe.Constraints.Function;
	
	/**
		An event handler fired whenever a `fetch` event occurs — when a `GlobalFetch.fetch` is called.
	**/
	var onfetch : haxe.Constraints.Function;
	
	/**
		An event handler fired whenever a `message` event occurs — when incoming messages are received. Controlled pages can use the `MessagePort.postMessage()` method to send messages to service workers. The service worker can optionally send a response back via the `MessagePort` exposed in `event.data.port`, corresponding to the controlled page.
	**/
	var onmessage : haxe.Constraints.Function;
	
	/**
		An event handler fired whenever a `push` event occurs — when a server push notification is received.
	**/
	var onpush : haxe.Constraints.Function;
	
	/**
		An event handler fired whenever a `pushsubscriptionchange` event occurs — when a push subscription has been invalidated, or is about to be invalidated (e.g. when a push service sets an expiration time.)
	**/
	var onpushsubscriptionchange : haxe.Constraints.Function;
	
	/**
		An event handler fired whenever a `notificationclick` event occurs — when a user clicks on a displayed notification.
	**/
	var onnotificationclick : haxe.Constraints.Function;
	
	/** @throws DOMError */
	
	/**
		Allows the current service worker registration to progress from waiting to active state while service worker clients are using it.
	**/
	function skipWaiting() : Promise<Void>;
}