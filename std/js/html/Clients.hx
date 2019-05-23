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

// This file is generated from mozilla\Clients.webidl. Do not edit!

package js.html;

import js.lib.Promise;

/**
	The `Clients` interface provides access to `Client` objects. Access it via ``self`.clients` within a service worker.

	Documentation [Clients](https://developer.mozilla.org/en-US/docs/Web/API/Clients) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/Clients$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/Clients>
**/
@:native("Clients")
extern class Clients {

	/**
		Returns a `Promise` for a `Client` matching a given `Client.id`.
	**/
	function get( id : String ) : Promise<Dynamic>;

	/**
		Returns a `Promise` for an array of `Client` objects. An options argument allows you to control the types of clients returned. 
	**/
	function matchAll( ?options : ClientQueryOptions ) : Promise<Array<Client>>;

	/**
		Opens a new browser window for a given url and returns a `Promise` for the new `WindowClient`.
	**/
	function openWindow( url : String ) : Promise<WindowClient>;

	/**
		Allows an active service worker to set itself as the `ServiceWorkerContainer.controller` for all clients within its `ServiceWorkerRegistration.scope`. 
	**/
	function claim() : Promise<Void>;
}