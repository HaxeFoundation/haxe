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

// This file is generated from mozilla\Client.webidl. Do not edit!

package js.html;

/**
	The `Client` interface of the ServiceWorker API represents the scope of a service worker client. A service worker client is either a document in a browser context or a `SharedWorker`, which is controlled by an active worker. A client object acts as a snapshot representation of its associated service worker client in the scope of a service worker.

	Documentation [Client](https://developer.mozilla.org/en-US/docs/Web/API/Client) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/Client$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/Client>
**/
@:native("Client")
extern class Client
{
	
	/**
		The URL of the current service worker client.
	**/
	var url(default,null) : String;
	
	/**
		Indicates the type of browsing context of the current client. This value can be one of `auxiliary`, `top-level`, `nested`, or `none`.
	**/
	var frameType(default,null) : FrameType;
	
	/**
		Returns the universally unique identifier of the `Client` object.
	**/
	var id(default,null) : String;
	
	/** @throws DOMError */
	
	/**
		Allows a service worker to send a message to a `ServiceWorkerClient`.
	**/
	function postMessage( message : Dynamic, ?transfer : Array<Dynamic> ) : Void;
}