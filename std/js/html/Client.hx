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

// This file is generated from mozilla\Client.webidl. Do not edit!

package js.html;

/**
	The `Client` interface represents an executable context such as a `Worker`, or a `SharedWorker`. `Window` clients are represented by the more-specific `WindowClient`. You can get `Client`/`WindowClient` objects from methods such as `Clients.matchAll()` and `Clients.get()`.

	Documentation [Client](https://developer.mozilla.org/en-US/docs/Web/API/Client) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/Client$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/Client>
**/
@:native("Client")
extern class Client {
	
	/**
		The URL of the client as a string.
	**/
	var url(default,null) : String;
	var frameType(default,null) : FrameType;
	
	/**
		The client's type as a string. It can be "`window"`, "`worker"`, or "`sharedworker"`.
	**/
	var type(default,null) : ClientType;
	
	/**
		The universally unique identifier of the client as a string.
	**/
	var id(default,null) : String;
	
	
	/**
		Sends a message to the client.
		@throws DOMError
	**/
	function postMessage( message : Dynamic, ?transfer : Array<Dynamic> ) : Void;
}