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

// This file is generated from mozilla\AbortController.webidl. Do not edit!

package js.html;

/**
	The `AbortController` interface represents a controller object that allows you to abort one or more DOM requests as and when desired.

	Documentation [AbortController](https://developer.mozilla.org/en-US/docs/Web/API/AbortController) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/AbortController$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/AbortController>
**/
@:native("AbortController")
extern class AbortController {
	
	/**
		Returns a `AbortSignal` object instance, which can be used to communicate with/abort a DOM request.
	**/
	var signal(default,null) : AbortSignal;
	
	/** @throws DOMError */
	function new() : Void;
	
	/**
		Aborts a DOM request before it has completed. This is able to abort fetch requests, consumption of any response `Body`, and streams.
	**/
	function abort() : Void;
}