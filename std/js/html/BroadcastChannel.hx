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

// This file is generated from mozilla\BroadcastChannel.webidl. Do not edit!

package js.html;

/**
	The `BroadcastChannel` interface represents a named channel that any browsing context of a given origin can subscribe to. It allows communication between different documents (in different windows, tabs, frames or iframes) of the same origin. Messages are broadcasted via a `message` event fired at all `BroadcastChannel` objects listening to the channel.

	Documentation [BroadcastChannel](https://developer.mozilla.org/en-US/docs/Web/API/BroadcastChannel) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/BroadcastChannel$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/BroadcastChannel>
**/
@:native("BroadcastChannel")
extern class BroadcastChannel extends EventTarget
{
	
	/**
		Returns a `DOMString`, the name of the channel.
	**/
	var name(default,null) : String;
	
	/**
		Is an `EventHandler` property that specifies the function to execute when a `message` event is fired on this object.
	**/
	var onmessage : haxe.Constraints.Function;
	
	/** @throws DOMError */
	function new( channel : String ) : Void;
	/** @throws DOMError */
	
	/**
		Sends the message, of any type of object, to each `BroadcastChannel` object listening to the same channel.
	**/
	function postMessage( message : Dynamic ) : Void;
	
	/**
		Closes the channel object, indicating it won't get any new messages, and allowing it to be, eventually, garbage collected.
	**/
	function close() : Void;
}