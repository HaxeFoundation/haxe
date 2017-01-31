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

// This file is generated from mozilla\Worker.webidl. Do not edit!

package js.html;

/**
	The `Worker` interface of the Web Workers API represents a background task that can be easily created and can send messages back to its creator. Creating a worker is as simple as calling the `Worker()` constructor and specifying a script to be run in the worker thread.

	Documentation [Worker](https://developer.mozilla.org/en-US/docs/Web/API/Worker) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/Worker$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/Worker>
**/
@:native("Worker")
extern class Worker extends EventTarget
{
	
	/**
		An `EventListener` called whenever a `MessageEvent` of type `message` bubbles through the worker — i.e. when a message is sent to the parent document from the worker via `DedicatedWorkerGlobalScope.postMessage`. The message is stored in the event's `MessageEvent.data` property.
	**/
	var onmessage : haxe.Constraints.Function;
	var onerror : haxe.Constraints.Function;
	
	/** @throws DOMError */
	function new( scriptURL : String ) : Void;
	
	/**
		Immediately terminates the worker. This does not offer the worker an opportunity to finish its operations; it is simply stopped at once. ServiceWorker instances do not support this method.
	**/
	function terminate() : Void;
	/** @throws DOMError */
	
	/**
		Sends a message — which can consist of `any` JavaScript object — to the worker's inner scope.
	**/
	function postMessage( message : Dynamic, ?transfer : Array<Dynamic> ) : Void;
}