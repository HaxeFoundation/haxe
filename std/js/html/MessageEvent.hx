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

// This file is generated from mozilla\MessageEvent.webidl. Do not edit!

package js.html;

/**
	A `MessageEvent` is the interface representing a message received by a target, being a `WebSocket` or a WebRTC `RTCDataChannel`

	Documentation [MessageEvent](https://developer.mozilla.org/en-US/docs/Web/API/MessageEvent) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/MessageEvent$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/MessageEvent>
**/
@:native("MessageEvent")
extern class MessageEvent extends Event
{
	
	/**
		Returns a `DOMString`, `Blob` or an `ArrayBuffer` containing the data send by the emitter.
	**/
	var data(default,null) : Dynamic;
	
	/**
		Is a `DOMString` …
	**/
	var origin(default,null) : String;
	var lastEventId(default,null) : String;
	
	/**
		…
	**/
	var source(default,null) : haxe.extern.EitherType<Window,MessagePort>;
	
	/**
		…
	**/
	var ports(default,null) : MessagePortList;
	
	/** @throws DOMError */
	function new( type : String, ?eventInitDict : MessageEventInit ) : Void;
	
	/**
		… Do not use this anymore: use the `MessageEvent.MessageEvent` constructor instead.
	**/
	function initMessageEvent( type : String, bubbles : Bool, cancelable : Bool, data : Dynamic, origin : String, lastEventId : String, source : haxe.extern.EitherType<Window,MessagePort>, ports : Array<MessagePort> ) : Void;
}