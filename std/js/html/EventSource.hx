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

// This file is generated from mozilla\EventSource.webidl. Do not edit!

package js.html;

/**
	The `EventSource` interface is used to receive server-sent events. It connects to a server over HTTP and receives events in `text/event-stream` format without closing the connection.

	Documentation [EventSource](https://developer.mozilla.org/en-US/docs/Web/API/EventSource) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/EventSource$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/EventSource>
**/
@:native("EventSource")
extern class EventSource extends EventTarget
{
	static inline var CONNECTING : Int = 0;
	static inline var OPEN : Int = 1;
	static inline var CLOSED : Int = 2;
	
	
	/**
		A `DOMString` representing the URL of the source.
	**/
	var url(default,null) : String;
	var withCredentials(default,null) : Bool;
	
	/**
		An `unsigned short` representing the state of the connection. Possible values are `CONNECTING` (`0`), `OPEN` (`1`), or `CLOSED` (`2`).
	**/
	var readyState(default,null) : Int;
	
	/**
		Is an `EventHandler` being called when an `open` event is received, that is when the connection was just opened.
	**/
	var onopen : haxe.Constraints.Function;
	
	/**
		Is an `EventHandler` being called when a `message` event is received, that is when a message is coming from the source.
	**/
	var onmessage : haxe.Constraints.Function;
	
	/**
		Is an `EventHandler` being called when an error occurs and the `error` event is dispatched on this object.
	**/
	var onerror : haxe.Constraints.Function;
	
	/** @throws DOMError */
	function new( url : String, ?eventSourceInitDict : EventSourceInit ) : Void;
	
	/**
		Closes the connection, if any, and sets the `readyState` attribute to `CLOSED`. If the connection is already closed, the method does nothing.
	**/
	function close() : Void;
}