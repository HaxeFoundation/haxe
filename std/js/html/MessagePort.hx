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

// This file is generated from mozilla\MessagePort.webidl. Do not edit!

package js.html;

/**
	The `MessagePort` interface of the Channel Messaging API represents one of the two ports of a `MessageChannel`, allowing messages to be sent from one port and listening out for them arriving at the other.

	Documentation [MessagePort](https://developer.mozilla.org/en-US/docs/Web/API/MessagePort) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/MessagePort$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/MessagePort>
**/
@:native("MessagePort")
extern class MessagePort extends EventTarget
{
	var onmessage : haxe.Constraints.Function;
	var onmessageerror : haxe.Constraints.Function;
	
	/** @throws DOMError */
	function postMessage( message : Dynamic, ?transferable : Array<Dynamic> = [] ) : Void;
	function start() : Void;
	function close() : Void;
}