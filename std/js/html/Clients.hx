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

// This file is generated from mozilla\Clients.webidl. Do not edit!

package js.html;

/**
	The `Clients` interface of the Service Workers API represents a container for a list of `Client` objects.

	Documentation [Clients](https://developer.mozilla.org/en-US/docs/Web/API/Clients) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/Clients$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/Clients>
**/
@:native("Clients")
extern class Clients
{
	
	/**
		Gets a service worker client matching a given `id` and returns it in a `Promise`.
	**/
	function get( id : String ) : Promise<Dynamic>;
	
	/**
		Gets a list of service worker clients and returns them in a `Promise`. Include the `options` parameter to return all service worker clients whose origin is the same as the associated service worker's origin. If `options` are not included, the method returns only the service worker clients controlled by the service worker. 
	**/
	function matchAll( ?options : ClientQueryOptions ) : Promise<Array<Client>>;
	
	/**
		Allows an active Service Worker to set itself as the active worker for a client page when the worker and the page are in the same scope. 
	**/
	function claim() : Promise<Void>;
}