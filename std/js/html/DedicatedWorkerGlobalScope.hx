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

// This file is generated from mozilla\DedicatedWorkerGlobalScope.webidl. Do not edit!

package js.html;

/**
	The `DedicatedWorkerGlobalScope` object (the `Worker` global scope) is accessible through the `self` keyword. Some additional global functions, namespaces objects, and constructors, not typically associated with the worker global scope, but available on it, are listed in the JavaScript Reference. See also: Functions available to workers.

	Documentation [DedicatedWorkerGlobalScope](https://developer.mozilla.org/en-US/docs/Web/API/DedicatedWorkerGlobalScope) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/DedicatedWorkerGlobalScope$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/DedicatedWorkerGlobalScope>
**/
@:native("DedicatedWorkerGlobalScope")
extern class DedicatedWorkerGlobalScope extends WorkerGlobalScope
{
	
	/**
		Is an `EventHandler` representing the code to be called when the `message` event is raised. These events are of type `MessageEvent` and will be called when the worker receives a message from the document that started it (i.e. from the `Worker.postMessage` method.)
	**/
	var onmessage : haxe.Constraints.Function;
	
	/** @throws DOMError */
	function postMessage( message : Dynamic, ?transfer : Array<Dynamic> ) : Void;
}