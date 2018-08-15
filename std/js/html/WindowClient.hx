/*
 * Copyright (C)2005-2018 Haxe Foundation
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
	The `WindowClient` interface of the ServiceWorker API represents the scope of a service worker client that is a document in a browser context, controlled by an active worker. The service worker client independently selects and uses a service worker for its own loading and sub-resources.

	Documentation [WindowClient](https://developer.mozilla.org/en-US/docs/Web/API/WindowClient) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/WindowClient$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/WindowClient>
**/
@:native("WindowClient")
extern class WindowClient extends Client
{
	
	/**
		Indicates the visibility of the current client. This value can be one of `hidden`, `visible`, `prerender`, or `unloaded`.
	**/
	var visibilityState(default,null) : VisibilityState;
	
	/**
		A boolean that indicates whether the current client has focus.
	**/
	var focused(default,null) : Bool;
	
	
	/**
		Gives user input focus to the current client. 
		@throws DOMError
	**/
	function focus() : Promise<WindowClient>;
	
	/**
		Loads a specified URL into a controlled client page.
		@throws DOMError
	**/
	function navigate( url : String ) : Promise<WindowClient>;
}